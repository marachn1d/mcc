use super::Identifier;
use super::InstructionSet;
use crate::compiler::parse;
use parse::BinaryOperator;
use parse::UnaryOperator;
use std::rc::Rc;

pub type Program = super::Program<Instruction>;
pub type Function = super::Function<Instruction>;

pub type TackyBinary = BinaryOperator;
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
}

#[derive(Clone)]
pub enum Value {
    Constant(u64),
    Var(Rc<Identifier>),
}
