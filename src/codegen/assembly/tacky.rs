use super::Identifier;
use super::InstructionSet;
use crate::lex::Constant;
use crate::parse;
use crate::semantics::StaticInit;
use parse::UnaryOperator;
use parse::VarType;

//pub type Program = super::Program<Instruction>;
//pub type FunctionDefinition = super::FunctionDefinition<Instruction>;

pub type TackyUnary = UnaryOperator;
impl InstructionSet for Instruction {}

#[derive(Debug)]
pub struct Program(pub Box<[TopLevel]>);

#[derive(Debug)]
pub enum TopLevel {
    Fn(FunctionDefinition),
    StaticVar(StaticVar),
}

#[derive(Debug)]
pub struct StaticVar {
    pub name: Identifier,
    pub global: bool,
    pub init: StaticInit,
    pub typ: VarType,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    // empty for x86, which kinda sucks,
    pub params: Box<[Identifier]>,
    pub global: bool,
    pub body: Box<[Instruction]>,
}

#[derive(Debug)]
pub enum Instruction {
    SignExtend {
        src: Value,
        dst: Value,
    },

    Truncate {
        src: Value,
        dst: Value,
    },
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
    Constant(Constant),
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
