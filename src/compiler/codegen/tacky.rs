use super::AstConstant;
use super::AstExpression;
use super::AstFunction;
use super::AstProgram;
use super::AstStatement;
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
    todo!()
    /*
    match expression {
        AstExpression::Constant(AstConstant(c)) => Value::Constant(c),
        AstExpression::Unary(AstUnary {
            exp: expression,
            op,
        }) => {
            let tmp = new_var();
            let expression_result = convert_expression(*expression, instructions);
            let unary = Instruction::Unary {
                op,
                source: expression_result,
                dst: tmp.clone(),
            };
            instructions.push(unary);
            Value::Var(tmp)
        }
        AstExpression::Nested(e) => convert_expression(*e, instructions),
    }
        */
}

fn new_var() -> Rc<Identifier> {
    let number = TEMP_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("tmp_{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}
