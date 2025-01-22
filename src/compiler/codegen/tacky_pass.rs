use super::assembly;
use super::Identifier;
use crate::compiler::parse;
use crate::compiler::parse::Unary as AstUnary;
use assembly::push_instructions;
use assembly::tacky::Instruction;
use assembly::tacky::TackyBinary;
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

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

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
            operator: TackyBinary::LogAnd,
            left,
            right,
        }) => {
            let source_1 = convert_expression(*left, instructions);
            let false_label = and_label();
            let end_label = end_label();
            let result = Value::Var(new_var());
            instructions.push(Instruction::JumpIfZero {
                condition: source_1,
                target: false_label.clone(),
            });
            let source_2 = convert_expression(*right, instructions);
            push_instructions(
                instructions,
                [
                    Instruction::JumpIfZero {
                        condition: source_2,
                        target: false_label.clone(),
                    },
                    Instruction::Copy {
                        src: Value::Constant(1),
                        dst: result.clone(),
                    },
                    Instruction::Jump {
                        target: end_label.clone(),
                    },
                    Instruction::Label(false_label),
                    Instruction::Copy {
                        src: Value::Constant(0),
                        dst: result.clone(),
                    },
                    Instruction::Label(end_label.clone()),
                ],
            );
            result
        }
        AstExpression::Binary(AstBinary {
            operator: TackyBinary::LogOr,
            left,
            right,
        }) => {
            let source_1 = convert_expression(*left, instructions);
            let true_label = or_label();
            let end_label = end_label();
            let result = Value::Var(new_var());
            // if source 1 is true we jump to true label
            instructions.push(Instruction::JumpIfNotZero {
                condition: source_1,
                target: true_label.clone(),
            });
            let source_2 = convert_expression(*right, instructions);
            push_instructions(
                instructions,
                [
                    Instruction::JumpIfNotZero {
                        condition: source_2,
                        target: true_label.clone(),
                    },
                    Instruction::Copy {
                        src: Value::Constant(0),
                        dst: result.clone(),
                    },
                    Instruction::Jump {
                        target: end_label.clone(),
                    },
                    Instruction::Label(true_label),
                    Instruction::Copy {
                        src: Value::Constant(1),
                        dst: result.clone(),
                    },
                    Instruction::Label(end_label),
                ],
            );
            result
        }
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

fn and_label() -> Rc<Identifier> {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("and_false{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}

fn or_label() -> Rc<Identifier> {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("or_true{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}

fn end_label() -> Rc<Identifier> {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("end{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}
