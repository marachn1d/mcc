use super::tacky::Value;
use super::Identifier;
use super::InstructionSet;
use super::Register;
use crate::parse::UnaryOperator;
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
        src: T,
        dst: T,
    },
    Movsx {
        ty: AsmType,
        src: T,
        dst: T,
    },
    Unary {
        operator: Unary,
        operand: T,
        ty: AsmType,
    },
    Binary {
        operator: Binary,
        op: T,
        dst_op: T,
        ty: AsmType,
    },
    Push(T),
    Call(Identifier),
    Ret,
    Idiv {
        divisor: T,
        ty: AsmType,
    },
    Cdq(AsmType),
    Cmp {
        left: T,
        right: T,
        ty: AsmType,
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

#[derive(Clone, Debug)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
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
            }
        )
    }
}

impl BaseX86<PseudoOp> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::Binary {
            operator: Binary::Sub,
            op: Op::Imm(n).pseudo(),
            dst_op: Op::Register(Register::Sp).pseudo(),
            ty: AsmType::Quadword,
        }
    }

    pub const fn deallocate_stack(n: i64) -> Self {
        Self::Binary {
            operator: Binary::Add,
            op: Op::Imm(n).pseudo(),
            dst_op: Op::Register(Register::Sp).pseudo(),
            ty: AsmType::Quadword,
        }
    }
}

impl BaseX86<Op> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::Binary {
            operator: Binary::Sub,
            op: Op::Imm(n),
            dst_op: Op::Register(Register::Sp),
            ty: AsmType::Quadword,
        }
    }

    pub const fn deallocate_stack(n: i64) -> Self {
        Self::Binary {
            operator: Binary::Add,
            op: Op::Imm(n),
            dst_op: Op::Register(Register::Sp),
            ty: AsmType::Quadword,
        }
    }
}

impl<T: Operand> BaseX86<T> {
    pub const fn mov(src: T, dst: T, ty: AsmType) -> Self {
        Self::Mov { dst, src, ty }
    }

    #[allow(dead_code)]
    pub const fn unary(operator: Unary, operand: T, ty: AsmType) -> Self {
        Self::Unary {
            operator,
            operand,
            ty,
        }
    }

    pub const fn binary(operator: Binary, op: T, dst_op: T, ty: AsmType) -> Self {
        Self::Binary {
            operator,
            op,
            dst_op,
            ty,
        }
    }

    pub const fn idiv(divisor: T, ty: AsmType) -> Self {
        Self::Idiv { divisor, ty }
    }

    pub const fn cmp(left: T, right: T, ty: AsmType) -> Self {
        Self::Cmp { left, right, ty }
    }

    pub const fn movsx(src: T, dst: T) -> Self {
        Self::Mov {
            ty: AsmType::Quadword,
            src,
            dst,
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

            Self::Mov { src, dst, ty } => write!(f, "mov{ty}  {src}, {dst}"),
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Unary {
                operator,
                operand,
                ty,
            } => write!(f, "{operator}{ty} {operand}"),
            Self::Binary {
                operator: operator @ (Binary::ShiftLeft | Binary::ShiftRight),
                op: Op::Register(r),
                dst_op,
                ty,
            } => {
                write!(f, "{operator}{ty} {reg}, {dst_op}", reg = r.one_byte())
            }
            Self::Binary {
                operator,
                op,
                dst_op,
                ty,
            } => {
                write!(f, "{operator}{ty} {op}, {dst_op}")
            }
            Self::Idiv { divisor, ty } => {
                write!(f, "idiv{ty} {divisor}")
            }
            Self::Cdq(AsmType::Longword) => {
                write!(f, "cdq")
            }
            Self::Cdq(AsmType::Quadword) => {
                write!(f, "cdo")
            }
            Self::Cmp { left, right, ty } => {
                write!(f, "cmp{ty} {left}, {right}")
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
            Self::Movsx { src, dst, ty: _ } => {
                write!(f, "movslq {src}, {dst}")
            }
        }
    }
}

impl Op {
    pub const fn pseudo(self) -> PseudoOp {
        PseudoOp::Normal(self)
    }
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
impl From<UnaryOperator> for Unary {
    fn from(op: UnaryOperator) -> Self {
        match op {
            UnaryOperator::Negate => Self::Neg,
            UnaryOperator::Complement => Self::Not,
            // PROBABLY BAD
            UnaryOperator::Not => unreachable!(),
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

impl Op {
    pub const REGISTER_TABLE: [Register; 6] = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ];
}

impl PseudoOp {
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
