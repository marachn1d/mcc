use ast::parse::{StaticInit, UnOp};
use ast::VarType;
use ast::{Constant, Ident};

//pub type Program = super::Program<Instruction>;
//pub type FunctionDefinition = super::FunctionDefinition<Instruction>;

#[derive(Debug)]
pub struct Program(pub Box<[TopLevel]>);

#[derive(Debug)]
pub enum TopLevel {
    Fn(FunctionDefinition),
    StaticVar(StaticVar),
}

#[derive(Debug)]
pub struct StaticVar {
    pub name: Ident,
    pub global: bool,
    pub init: StaticInit,
    pub typ: VarType,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Ident,
    // empty for x86, which kinda sucks,
    pub params: Box<[Ident]>,
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
        op: UnOp,
        source: Value,
        dst: Ident,
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
        target: Ident,
    },
    JumpIfZero {
        condition: Value,
        target: Ident,
    },
    JumpIfNotZero {
        condition: Value,
        target: Ident,
    },
    Label(Ident),
    FunCall {
        name: Ident,
        args: Box<[Value]>,
        dst: Value,
    },
}

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Var(Ident),
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
