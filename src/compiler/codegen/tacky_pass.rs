use super::assembly;
use super::Identifier;
use crate::compiler::parse;
use crate::compiler::parse::Unary as AstUnary;
use assembly::tacky::Instruction;
use assembly::tacky::Value;

use assembly::tacky::Function;
use assembly::tacky::Program;
use parse::Binary as AstBinary;
use parse::Expression as AstExpression;
use parse::Factor as AstFactor;
use parse::Function as AstFunction;
use parse::Program as AstProgram;
use parse::Statement as AstStatement;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

static TEMP_COUNT: AtomicUsize = AtomicUsize::new(0);

pub fn emit(program: AstProgram) -> Program {
    super::Program::<Instruction>(convert_function(program.0))
}

fn convert_function(function: AstFunction) -> Function {
    Function {
        name: function.name,
        body: convert_statement(function.body),
    }
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
