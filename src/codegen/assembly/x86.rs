use super::tacky::Value;
use super::Identifier;
use super::InstructionSet;
use super::Register;
use crate::parse::UnOp;
use std::fmt::{self, Display, Formatter};

pub type Pseudo = BaseX86<PseudoOp>;
pub type X86 = BaseX86<Op>;
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AsmType {
    Longword,
    Quadword,
}
impl InstructionSet for X86 {}
impl InstructionSet for Pseudo {}

#[derive(Clone, Debug)]
pub enum BaseX86<T: Operand> {
    Mov {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Movsx {
        ty: AsmType,
        regs: OpPair<T>,
    },

    Unary {
        operator: Unary,
        operand: T,
        ty: AsmType,
    },
    Binary {
        operator: Binary,
        regs: OpPair<T>,
        ty: AsmType,
    },
    Push(T),
    Call(Identifier),
    Ret,
    Idiv {
        divisor: T,
        ty: AsmType,
    },
    Div {
        divisor: T,
        ty: AsmType,
    },
    Cdq(AsmType),
    Movzx {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Cmp {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Jmp(Identifier),
    JmpCC {
        condition: CondCode,
        label: Identifier,
    },
    SetCC {
        condition: CondCode,
        op: T,
    },
    Label(Identifier),
}

pub type OpPair<T> = (T, T);

#[derive(Clone, Debug)]
pub enum CondCode {
    E,
    NE,
    //signed comparisons
    G,
    GE,
    L,
    LE,

    //unsigned comparisons
    A, // A > B
    AE,
    B, // A < B
    BE,
}

impl Display for CondCode {
    fn fmt(&self, writer: &mut Formatter) -> fmt::Result {
        write!(
            writer,
            "{}",
            match self {
                Self::E => "e",
                Self::NE => "ne",
                Self::G => "g",
                Self::GE => "ge",
                Self::L => "l",
                Self::LE => "le",
                Self::A => "a",
                Self::AE => "ae",
                Self::B => "b",
                Self::BE => "be",
            }
        )
    }
}

impl BaseX86<PseudoOp> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::Imm(n).pseudo(),
            Op::Register(Register::Sp).pseudo(),
            AsmType::Quadword,
        )
    }

    pub const fn deallocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Add,
            Op::Imm(n).pseudo(),
            Op::Register(Register::Sp).pseudo(),
            AsmType::Quadword,
        )
    }
}

impl BaseX86<Op> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::Imm(n),
            Op::Register(Register::Sp),
            AsmType::Quadword,
        )
    }
}

impl<T: Operand> BaseX86<T> {
    pub const fn mov(src: T, dst: T, ty: AsmType) -> Self {
        Self::Mov {
            regs: (src, dst),
            ty,
        }
    }

    pub const fn cmp(s1: T, s2: T, ty: AsmType) -> Self {
        Self::Cmp { regs: (s1, s2), ty }
    }

    #[allow(dead_code)]
    pub const fn unary(operator: Unary, operand: T, ty: AsmType) -> Self {
        Self::Unary {
            operator,
            operand,
            ty,
        }
    }

    pub const fn binary(operator: Binary, op: T, dst: T, ty: AsmType) -> Self {
        Self::Binary {
            operator,
            regs: (op, dst),
            ty,
        }
    }

    pub const fn idiv(divisor: T, ty: AsmType) -> Self {
        Self::Idiv { divisor, ty }
    }

    pub const fn div(divisor: T, ty: AsmType) -> Self {
        Self::Div { divisor, ty }
    }

    pub const fn movsx(src: T, dst: T) -> Self {
        Self::Movsx {
            ty: AsmType::Quadword,
            regs: (src, dst),
        }
    }

    pub const fn movzx(src: T, dst: T) -> Self {
        Self::Movzx {
            regs: (src, dst),
            ty: AsmType::Quadword,
        }
    }
}

pub trait Operand {}

impl Display for AsmType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Longword => "l",
            Self::Quadword => "q",
        })
    }
}

impl Display for X86 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call(fun) => write!(f, "call _{fun}"),

            Self::Push(Op::Register(r)) => write!(f, "pushq {}", r.eight_byte()),
            Self::Push(op) => write!(f, "pushq {op}"),

            Self::Mov {
                regs: (src, dst),
                ty,
            } => {
                write!(f, "mov{ty}  {}, {}", src.sized_fmt(*ty), dst.sized_fmt(*ty))
            }
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Unary {
                operator,
                operand,
                ty,
            } => write!(f, "{operator}{ty} {}", operand.sized_fmt(*ty)),
            Self::Binary {
                operator: operator @ (Binary::ShiftLeft | Binary::ShiftRight),
                regs: (Op::Register(r), by),
                ty,
            } => {
                write!(f, "{operator}{ty} {}, {}", r.one_byte(), by.sized_fmt(*ty),)
            }
            Self::Binary {
                operator,
                regs: (op, dst_op),
                ty,
            } => {
                write!(
                    f,
                    "{operator}{ty} {}, {}",
                    op.sized_fmt(*ty),
                    dst_op.sized_fmt(*ty)
                )
            }
            Self::Idiv { divisor, ty } => {
                write!(f, "idiv{ty} {}", divisor.sized_fmt(*ty))
            }
            Self::Div { divisor, ty } => {
                write!(f, "div{ty} {}", divisor.sized_fmt(*ty))
            }
            Self::Cdq(AsmType::Longword) => {
                write!(f, "cdq")
            }
            Self::Cdq(AsmType::Quadword) => {
                write!(f, "cqo")
            }
            Self::Cmp {
                regs: (left, right),
                ty,
            } => {
                write!(
                    f,
                    "cmp{ty} {}, {}",
                    left.sized_fmt(*ty),
                    right.sized_fmt(*ty)
                )
            }
            Self::Jmp(label) => {
                write!(f, "jmp L{label}")
            }
            Self::JmpCC { label, condition } => {
                write!(f, "j{condition} L{label}")
            }
            Self::SetCC { op, condition } => {
                write!(f, "set{condition} {op}")
            }
            Self::Label(label) => {
                write!(f, "L{label}:")
            }
            Self::Movsx {
                regs: (src, dst),
                ty,
            } => {
                write!(
                    f,
                    "movslq {}, {}",
                    src.sized_fmt(AsmType::Longword),
                    dst.sized_fmt(*ty)
                )
            }
            Self::Movzx {
                regs: (src, dst),
                ty,
            } => {
                write!(
                    f,
                    "movzlq {}, {}",
                    src.sized_fmt(AsmType::Longword),
                    dst.sized_fmt(*ty)
                )
            }
        }
    }
}

impl Op {
    pub const fn pseudo(self) -> PseudoOp {
        PseudoOp::Normal(self)
    }
}

pub mod op_regs {
    use super::Op;
    use super::Register;
    pub const AX: Op = Op::Register(Register::Ax);
    pub const CX: Op = Op::Register(Register::Cx);
    pub const DX: Op = Op::Register(Register::Dx);
    pub const DI: Op = Op::Register(Register::Di);
    pub const SI: Op = Op::Register(Register::Si);
    pub const R8: Op = Op::Register(Register::R8);
    pub const R9: Op = Op::Register(Register::R9);
    pub const R10: Op = Op::Register(Register::R10);
    pub const R11: Op = Op::Register(Register::R11);
    pub const SP: Op = Op::Register(Register::Sp);
}

#[derive(Clone, Debug)]
pub enum Op {
    Imm(i64),
    Register(Register),
    Stack(isize),
    Data(Identifier),
}
impl Operand for Op {}

impl Operand for PseudoOp {}

impl From<Register> for Op {
    fn from(reg: Register) -> Self {
        Self::Register(reg)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Unary {
    Not,
    Neg,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Binary {
    Add,
    Sub,
    Mult,
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight,
}

impl Display for Unary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            (match self {
                Self::Not => "not",
                Self::Neg => "neg",
            })
        )
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mult => "imul",
            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::ShiftLeft => "sal",
            Self::ShiftRight => "sar",
        })
    }
}
impl From<UnOp> for Unary {
    fn from(op: UnOp) -> Self {
        match op {
            UnOp::Negate => Self::Neg,
            UnOp::Complement => Self::Not,
            // PROBABLY BAD
            UnOp::Not => unreachable!(),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Imm(val) => write!(f, "${val}"),
            Self::Register(r) => write!(f, "{}", r.extended()),
            Self::Stack(n) => write!(f, "{n}(%rbp)"),
            Self::Data(name) => write!(f, "_{name}(%rip)"),
        }
    }
}

impl Op {
    fn sized_fmt(&self, size: AsmType) -> String {
        if let Self::Register(r) = self {
            match size {
                AsmType::Longword => r.extended().into(),
                AsmType::Quadword => r.eight_byte().into(),
            }
        } else {
            format!("{self}")
        }
    }
}

#[derive(Clone, Debug)]
pub enum PseudoOp {
    Normal(Op),
    PseudoRegister(Identifier),
}
impl From<Value> for PseudoOp {
    fn from(val: Value) -> Self {
        match val {
            Value::Constant(c) => Self::imm(c.long()),

            Value::Var(v) => Self::PseudoRegister(v),
        }
    }
}

impl From<Identifier> for PseudoOp {
    fn from(ident: Identifier) -> Self {
        Self::PseudoRegister(ident.clone())
    }
}

impl<T: Into<Op>> From<T> for PseudoOp {
    fn from(val: T) -> Self {
        let operand: Op = val.into();
        match operand {
            Op::Imm(a) => Self::imm(a),
            Op::Register(a) => Self::register(a),
            Op::Stack(a) => Self::stack(a),
            Op::Data(a) => Self::data(a),
        }
    }
}

#[allow(dead_code)]
pub mod pseudo_regs {
    use super::op_regs as regs;
    use super::PseudoOp as Psu;
    pub const AX: Psu = Psu::Normal(regs::AX);
    pub const CX: Psu = Psu::Normal(regs::CX);
    pub const DX: Psu = Psu::Normal(regs::DX);
    pub const DI: Psu = Psu::Normal(regs::DI);
    pub const SI: Psu = Psu::Normal(regs::SI);
    pub const R8: Psu = Psu::Normal(regs::R8);
    pub const R9: Psu = Psu::Normal(regs::R9);
    pub const R10: Psu = Psu::Normal(regs::R10);
    pub const R11: Psu = Psu::Normal(regs::R11);
    pub const SP: Psu = Psu::Normal(regs::SP);
}

impl PseudoOp {
    pub const SYSV_ARG_REGS: [Register; 6] = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ];
    pub const ZERO: Self = Self::Normal(Op::Imm(0));

    pub const fn imm(value: i64) -> Self {
        Self::Normal(Op::Imm(value))
    }

    pub const fn register(reg: Register) -> Self {
        Self::Normal(Op::Register(reg))
    }

    pub const fn data(name: Identifier) -> Self {
        Self::Normal(Op::Data(name))
    }

    pub const fn stack(offset: isize) -> Self {
        Self::Normal(Op::Stack(offset))
    }
}
