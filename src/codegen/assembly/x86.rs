use super::tacky::Value;
use super::Identifier;
use super::InstructionSet;
use super::Register;
use crate::parse::UnaryOperator;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

pub type Pseudo = BaseX86<PseudoOp>;
pub type X86 = BaseX86<Op>;

impl InstructionSet for X86 {}
impl InstructionSet for Pseudo {}

#[derive(Clone, Debug)]
pub enum BaseX86<T: Operand> {
    Mov {
        src: T,
        dst: T,
    },
    Unary {
        operator: Unary,
        operand: T,
    },
    Binary {
        operator: Binary,
        op: T,
        dst_op: T,
    },
    AllocateStack(usize),
    DeallocateStack(usize),
    Push(T),
    Call(Rc<Identifier>),
    Ret,
    Idiv {
        divisor: T,
    },
    Cdq,
    Cmp {
        left: T,
        right: T,
    },
    Jmp(Rc<Identifier>),
    JmpCC {
        condition: CondCode,
        label: Rc<Identifier>,
    },
    SetCC {
        condition: CondCode,
        op: T,
    },
    Label(Rc<Identifier>),
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

    pub const fn cmp(left: T, right: T) -> Self {
        Self::Cmp { left, right }
    }
}

pub trait Operand {}

impl Display for X86 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call(fun) => write!(f, "call _{fun}"),
            Self::DeallocateStack(size) => write!(f, "addq {size}, %rsp"),

            Self::Push(Op::Register(r)) => write!(f, "pushq {}", r.eight_byte()),
            Self::Push(op) => write!(f, "pushq {op}"),

            Self::Mov { src, dst } => write!(f, "movl  {src}, {dst}"),
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Unary { operator, operand } => write!(f, "{operator} {operand}"),
            Self::AllocateStack(amnt) => {
                write!(f, "subq ${amnt}, %rsp")
            }
            Self::Binary {
                operator: operator @ (Binary::ShiftLeft | Binary::ShiftRight),
                op: Op::Register(r),
                dst_op,
            } => {
                write!(f, "{operator} {reg}, {dst_op}", reg = r.one_byte())
            }
            Self::Binary {
                operator,
                op,
                dst_op,
            } => {
                write!(f, "{operator} {op}, {dst_op}")
            }
            Self::Idiv { divisor } => {
                write!(f, "idivl {divisor}")
            }
            Self::Cdq => {
                write!(f, "cdq")
            }
            Self::Cmp { left, right } => {
                write!(f, "cmpl {left}, {right}")
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
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Op {
    Imm(u64),
    Register(Register),
    Stack(usize),
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
                Self::Not => "notl",
                Self::Neg => "negl",
            })
        )
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "addl",
            Self::Sub => "subl",
            Self::Mult => "imull",
            Self::And => "andl",
            Self::Or => "orl",
            Self::Xor => "xorl",
            Self::ShiftLeft => "sall",
            Self::ShiftRight => "sarl",
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
            Self::Stack(n) => write!(f, "-{n}(%rbp)"),
        }
    }
}

#[derive(Clone, Debug)]
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

impl Op {
    pub const REGISTER_TABLE: [Register; 6] = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ];

    pub const fn param_dst(param_index: usize, params_len: usize) -> Self {
        match param_index {
            i @ 0..=5 => Self::Register(Self::REGISTER_TABLE[i]),
            i @ 6.. => {
                // the last one is gonna be 24 (stack already has rbp and maybe padding)
                // second to last is gonna be 32
                let distance_from_last = params_len - i + 1;
                // if we're the last one then it'll be 1, second to last it'll be 2
                // last one should be 3 because 8 * 3 = 24;
                Self::Stack(distance_from_last * 8)
            }
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

    pub const fn stack(offset: usize) -> Self {
        Self::Normal(Op::Stack(offset))
    }
}
