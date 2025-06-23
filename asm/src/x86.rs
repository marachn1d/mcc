mod register;
use super::tacky::Val;
use ast::UnOp;

use register::Register;

use ast::Key;
use std::fmt::{self, Display, Formatter};
pub type KeyTy = String;

// for now we're gonna go expensive
pub type Pseu = BaseX86<PseuOp<KeyTy>, KeyTy>;
pub type X86 = BaseX86<Op<KeyTy>, KeyTy>;

pub type OpPair<T> = (T, T);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AsmType {
    Longword,
    Quadword,
}

#[derive(Clone, Debug)]
pub enum BaseX86<T, Key: AsRef<str>> {
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
    Call(Key),
    Ret,
    Idiv {
        divisor: T,
        ty: AsmType,
    },
    Cdq(AsmType),
    Cmp {
        ty: AsmType,
        regs: OpPair<T>,
    },
    Jmp(Key),
    JmpCC {
        condition: CondCode,
        label: Key,
    },
    SetCC {
        condition: CondCode,
        op: T,
    },
    Label(Key),
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

impl Pseu {
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

impl<K: AsRef<str>> BaseX86<Op<K>, K> {
    pub const fn allocate_stack(n: i64) -> Self {
        Self::binary(
            Binary::Sub,
            Op::Imm(n),
            Op::Register(Register::Sp),
            AsmType::Quadword,
        )
    }
}

impl<T, K: AsRef<str>> BaseX86<T, K> {
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

    pub const fn movsx(src: T, dst: T) -> Self {
        Self::Movsx {
            ty: AsmType::Quadword,
            regs: (src, dst),
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
        }
    }
}

impl<Key: AsRef<str>> Op<Key> {
    pub const fn pseudo(self) -> PseuOp<Key> {
        PseuOp::Normal(self)
    }
}

pub mod op_regs {
    use super::KeyTy;
    use super::Op;
    use super::Register;
    pub const AX: Op<KeyTy> = Op::Register(Register::Ax);
    pub const CX: Op<KeyTy> = Op::Register(Register::Cx);
    pub const DX: Op<KeyTy> = Op::Register(Register::Dx);
    pub const DI: Op<KeyTy> = Op::Register(Register::Di);
    pub const SI: Op<KeyTy> = Op::Register(Register::Si);
    pub const R8: Op<KeyTy> = Op::Register(Register::R8);
    pub const R9: Op<KeyTy> = Op::Register(Register::R9);
    pub const R10: Op<KeyTy> = Op::Register(Register::R10);
    pub const R11: Op<KeyTy> = Op::Register(Register::R11);
    pub const SP: Op<KeyTy> = Op::Register(Register::Sp);
}

#[derive(Clone, Debug)]
pub enum Op<Key: AsRef<str>> {
    Imm(i64),
    Register(Register),
    Stack(isize),
    Data(Key),
}

impl<Key: AsRef<str>> From<Register> for Op<Key> {
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

impl<K: AsRef<str>> Display for Op<K> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Imm(val) => write!(f, "${val}"),
            Self::Register(r) => write!(f, "{}", r.extended()),
            Self::Stack(n) => write!(f, "{n}(%rbp)"),
            Self::Data(name) => write!(f, "_{}(%rip)", name.as_ref()),
        }
    }
}

impl<K: AsRef<str>> Op<K> {
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
pub enum PseuOp<K: AsRef<str>> {
    Normal(Op<K>),
    Pseu(K),
}
impl<K> From<Val<'_>> for PseuOp<K>
where
    for<'a> K: AsRef<str> + From<&'a str>,
{
    fn from(val: Val) -> Self {
        use ast::Constant;
        match val {
            Val::Constant(Constant::Long(x)) => Self::imm(x),

            Val::Var(v) => Self::Pseu(K::from(v.as_ref())),
            Val::Tmp(k) => Self::Pseu(K::from(k.as_ref())),

            _ => todo!(),
        }
    }
}

impl<K: AsRef<str>> From<K> for PseuOp<K> {
    fn from(ident: K) -> Self {
        Self::Pseu(ident)
    }
}

/*
impl<K: AsRef<str>, T: Into<Op<K>>> From<T> for PseuOp<K> {
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
*/

#[allow(dead_code)]
pub mod pseudo_regs {
    use super::op_regs as regs;
    use super::KeyTy;
    type Psu = super::PseuOp<KeyTy>;
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

impl<K: AsRef<str>> PseuOp<K> {
    pub const SYSV_ARG_REGS: [Register; 6] = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ];

    pub const fn imm(value: i64) -> Self {
        Self::Normal(Op::Imm(value))
    }
    pub const fn register(reg: Register) -> Self {
        Self::Normal(Op::Register(reg))
    }

    pub const fn data(name: K) -> Self {
        Self::Normal(Op::Data(name))
    }

    pub const fn stack(offset: isize) -> Self {
        Self::Normal(Op::Stack(offset))
    }
}
