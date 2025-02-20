use super::Identifier;
use super::InstructionSet;
use crate::parse;
use parse::UnaryOperator;
use std::rc::Rc;

pub type Program = super::Program<Instruction>;
pub type FunctionDefinition = super::FunctionDefinition<Instruction>;

pub type TackyUnary = UnaryOperator;
impl InstructionSet for Instruction {}

pub enum Instruction {
    Return(Value),
    Unary {
        op: TackyUnary,
        source: Value,
        dst: Rc<Identifier>,
    },
    Binary {
        operator: TackyBinary,
        source_1: Value,
        source_2: Value,
        dst: Value,
    },
    Copy {
        src: Value,
        dst: Value,
    },
    Jump {
        target: Rc<Identifier>,
    },
    JumpIfZero {
        condition: Value,
        target: Rc<Identifier>,
    },
    JumpIfNotZero {
        condition: Value,
        target: Rc<Identifier>,
    },
    Label(Rc<Identifier>),
    FunCall {
        name: Rc<Identifier>,
        args: Box<[Value]>,
        dst: Value,
    },
}

#[derive(Clone)]
pub enum Value {
    Constant(u64),
    Var(Rc<Identifier>),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TackyBinary {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LeftShift,
    RightShift,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}
