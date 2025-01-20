use super::tacky::Value;
use super::Identifier;
use super::InstructionSet;
use super::Register;
use crate::compiler::parse::BinaryOperator;
use crate::compiler::parse::UnaryOperator;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::rc::Rc;

pub type Pseudo = BaseX86<PseudoOp>;
pub type X86 = BaseX86<Op>;

impl InstructionSet for X86 {}
impl InstructionSet for Pseudo {}

#[derive(Clone)]
pub enum BaseX86<T: Operand> {
    Mov { src: T, dst: T },
    Unary { operator: Unary, operand: T },
    Binary { operator: Binary, op: T, dst_op: T },
    AllocateStack(isize),
    Ret,
    Idiv { divisor: T },
    Cdq,
}

impl<T: Operand> BaseX86<T> {
    pub const fn mov(src: T, dst: T) -> Self {
        Self::Mov { dst, src }
    }

    #[allow(dead_code)]
    pub const fn unary(operator: Unary, operand: T) -> Self {
        Self::Unary { operator, operand }
    }

    pub const fn binary(operator: Binary, op: T, dst_op: T) -> Self {
        Self::Binary {
            operator,
            op,
            dst_op,
        }
    }

    pub const fn idiv(divisor: T) -> Self {
        Self::Idiv { divisor }
    }
}

pub trait Operand {}

impl X86 {
    pub fn emit(&self, writer: &mut impl Write) {
        let _ = match self {
            Self::Mov { src, dst } => write!(writer, "movl  {src}, {dst}"),
            Self::Ret => write!(writer, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Unary { operator, operand } => {
                operator.emit(writer);
                write!(writer, " {operand}")
            }
            Self::AllocateStack(amnt) => {
                write!(writer, "subq ${amnt}, %rsp")
            }
            Self::Binary {
                operator,
                op,
                dst_op,
            } => {
                operator.emit(writer);
                write!(writer, " {op}, {dst_op}")
            }
            Self::Idiv { divisor } => {
                write!(writer, "idivl {divisor}")
            }
            Self::Cdq => {
                write!(writer, "cdq")
            }
        };
    }
}

#[derive(Clone, Copy)]
pub enum Op {
    Imm(u64),
    Register(Register),
    Stack(isize),
}
impl Operand for Op {}

impl Operand for PseudoOp {}

impl From<Register> for Op {
    fn from(reg: Register) -> Self {
        Self::Register(reg)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Unary {
    Not,
    Neg,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Binary {
    Add,
    Sub,
    Mult,
}

impl Unary {
    fn emit(&self, writer: &mut impl Write) {
        let _ = writer.write_all(match self {
            Self::Not => b"notl",
            Self::Neg => b"negl",
        });
    }
}

impl Binary {
    fn emit(&self, writer: &mut impl Write) {
        let _ = writer.write_all(match self {
            Self::Add => b"addl",
            Self::Sub => b"subl",
            Self::Mult => b"imull",
        });
    }
}
impl From<UnaryOperator> for Unary {
    fn from(op: UnaryOperator) -> Self {
        match op {
            UnaryOperator::Negate => Self::Neg,
            UnaryOperator::Complement => Self::Not,
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Imm(val) => write!(f, "${}", val),
            Self::Register(r) => r.fmt(f),
            Self::Stack(n) => write!(f, "{}(%rbp)", n),
        }
    }
}

#[derive(Clone)]
pub enum PseudoOp {
    Normal(Op),
    PseudoRegister(Rc<Identifier>),
}

impl From<Value> for PseudoOp {
    fn from(val: Value) -> Self {
        match val {
            Value::Constant(c) => Self::imm(c),
            Value::Var(v) => Self::PseudoRegister(v),
        }
    }
}

impl From<Rc<Identifier>> for PseudoOp {
    fn from(ident: Rc<Identifier>) -> Self {
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
        }
    }
}

impl PseudoOp {
    pub const fn imm(value: u64) -> Self {
        Self::Normal(Op::Imm(value))
    }
    pub const fn register(reg: Register) -> Self {
        Self::Normal(Op::Register(reg))
    }

    pub const fn stack(offset: isize) -> Self {
        Self::Normal(Op::Stack(offset))
    }
}
