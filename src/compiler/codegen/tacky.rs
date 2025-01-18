use super::AstBinary;
use super::AstConstant;
use super::AstExpression;
use super::AstFactor;
use super::AstFunction;
use super::AstProgram;
use super::AstStatement;
use super::BinaryOperator;
use super::Identifier;
use super::UnaryOperator;
use crate::compiler::parse::Unary as AstUnary;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

static TEMP_COUNT: AtomicUsize = AtomicUsize::new(0);

pub struct Program(pub Function);

pub fn emit(program: AstProgram) -> Program {
    Program(convert_function(program.0))
}

pub struct Function {
    pub name: Identifier,
    pub body: Box<[Instruction]>,
}

fn convert_function(function: AstFunction) -> Function {
    Function {
        name: function.name,
        body: convert_statement(function.body),
    }
}

pub enum Instruction {
    Return(Value),
    Unary {
        op: UnaryOperator,
        source: Value,
        dst: Rc<Identifier>,
    },
    Binary {
        operator: BinaryOperator,
        source_1: Value,
        source_2: Value,
        dst: Value,
    },
}
#[derive(Clone)]
pub enum Value {
    Constant(u64),
    Var(Rc<Identifier>),
}

fn convert_statement(statement: AstStatement) -> Box<[Instruction]> {
    let mut instructions = Vec::new();
    let result = convert_expression(statement.ret, &mut instructions);
    instructions.push(Instruction::Return(result));
    instructions.into()
}

fn convert_expression(expression: AstExpression, instructions: &mut Vec<Instruction>) -> Value {
    match expression {
        AstExpression::Binary(AstBinary {
            operator,
            left,
            right,
        }) => {
            let source_1 = convert_expression(*left, instructions);
            let source_2 = convert_expression(*right, instructions);
            let dst = Value::Var(new_var());
            let binary = Instruction::Binary {
                operator,
                source_1,
                source_2,
                dst: dst.clone(),
            };
            instructions.push(binary);
            dst
        }
        AstExpression::Factor(factor) => convert_factor(factor, instructions),
    }
}

fn convert_factor(factor: AstFactor, instructions: &mut Vec<Instruction>) -> Value {
    match factor {
        AstFactor::Int(c) => Value::Constant(c),
        AstFactor::Unary(AstUnary { exp: factor, op }) => {
            let tmp = new_var();
            let factor_result = convert_factor(*factor, instructions);
            let unary = Instruction::Unary {
                op,
                source: factor_result,
                dst: tmp.clone(),
            };
            instructions.push(unary);
            Value::Var(tmp)
        }
        AstFactor::Nested(e) => convert_expression(*e, instructions),
    }
}

fn new_var() -> Rc<Identifier> {
    let number = TEMP_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("tmp_{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}
