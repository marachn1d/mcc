use super::Identifier;
use super::InstructionSet;
use crate::parse;
use parse::UnaryOperator;

pub type Program = super::Program<Instruction>;
pub type FunctionDefinition = super::FunctionDefinition<Instruction>;

pub type TackyUnary = UnaryOperator;
impl InstructionSet for Instruction {}

#[derive(Debug)]
pub enum Instruction {
    Return(Value),
    Unary {
        op: TackyUnary,
        source: Value,
        dst: Identifier,
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
        target: Identifier,
    },
    JumpIfZero {
        condition: Value,
        target: Identifier,
    },
    JumpIfNotZero {
        condition: Value,
        target: Identifier,
    },
    Label(Identifier),
    FunCall {
        name: Identifier,
        args: Box<[Value]>,
        dst: Value,
    },
}

#[derive(Clone, Debug)]
pub enum Value {
    Constant(u64),
    Var(Identifier),
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
