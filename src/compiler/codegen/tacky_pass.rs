use super::assembly;
use super::Identifier;
use crate::compiler::parse;
use crate::compiler::parse::Unary as AstUnary;
use assembly::tacky::Function;
use assembly::tacky::Instruction;
use assembly::tacky::Program;
use assembly::tacky::TackyBinary;
use assembly::tacky::Value;
use assembly::OpVec;
use parse::Binary as AstBinary;
use parse::BinaryOperator as AstBinop;
use parse::Declaration as AstDeclaration;
use parse::Expression as AstExpression;
use parse::Factor as AstFactor;
use parse::Function as AstFunction;
use parse::Program as AstProgram;

use parse::BlockItem as AstBlock;
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
    let mut body = convert_blocks(function.body);
    body.push_one(Instruction::Return(Value::Constant(0)));

    Function {
        name: function.name,
        body: body.into(),
    }
}

fn convert_blocks(block: Box<[AstBlock]>) -> OpVec<Instruction> {
    let mut instructions = OpVec::new();
    for block in block {
        match block {
            AstBlock::S(statement) => convert_statement(statement, &mut instructions),
            AstBlock::D(declaration) => convert_declaration(declaration, &mut instructions),
        };
    }
    instructions.push_one(Instruction::Return(Value::Constant(0)));
    instructions
}

fn convert_statement(statement: AstStatement, instructions: &mut OpVec<Instruction>) {
    match statement {
        AstStatement::Ret(e) => {
            let result = convert_expression(e, instructions);
            instructions.push_one(Instruction::Return(result));
        }
        AstStatement::Null => {}
        AstStatement::Exp(e) => {
            let _ = convert_expression(e, instructions);
        }
        AstStatement::If {
            condition,
            then,
            r#else: None,
        } => {
            let c = convert_expression(condition, instructions);
            let label = if_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: label.clone(),
            });
            convert_statement(*then, instructions);
            instructions.push_one(Instruction::Label(label));
        }
        AstStatement::If {
            condition,
            then,
            r#else: Some(r#else),
        } => {
            let c = convert_expression(condition, instructions);
            let else_label = else_label();
            let end = if_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: else_label.clone(),
            });
            convert_statement(*then, instructions);
            instructions.push([
                Instruction::Jump {
                    target: end.clone(),
                },
                Instruction::Label(else_label),
            ]);
            convert_statement(*r#else, instructions);
            instructions.push_one(Instruction::Label(end));
        }
    }
}

fn convert_declaration(dec: AstDeclaration, instructions: &mut OpVec<Instruction>) {
    if let Some(init) = dec.init {
        let result = convert_expression(init, instructions);
        instructions.push_one(Instruction::Copy {
            src: result,
            dst: Value::Var(dec.name),
        })
    }
}

fn convert_expression(expression: AstExpression, instructions: &mut OpVec<Instruction>) -> Value {
    match expression {
        AstExpression::Assignment(v) => {
            let (var, rhs) = *v;
            let AstExpression::Var(var) = var else {
                unreachable!()
            };
            let result = convert_expression(rhs, instructions);
            let var = Value::Var(var);
            instructions.push_one(Instruction::Copy {
                src: result,
                dst: var.clone(),
            });
            var
        }
        AstExpression::Binary(AstBinary {
            operator,
            left,
            right,
        }) => match process_binop(operator) {
            ProcessedBinop::LogAnd => {
                let source_1 = convert_expression(*left, instructions);
                let false_label = and_label();
                let end_label = end_label();
                let result = Value::Var(new_var());
                instructions.push_one(Instruction::JumpIfZero {
                    condition: source_1,
                    target: false_label.clone(),
                });
                let source_2 = convert_expression(*right, instructions);
                instructions.push([
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
                ]);
                result
            }
            ProcessedBinop::LogOr => {
                let source_1 = convert_expression(*left, instructions);
                let true_label = or_label();
                let end_label = end_label();
                let result = Value::Var(new_var());
                // if source 1 is true we jump to true label
                instructions.push_one(Instruction::JumpIfNotZero {
                    condition: source_1,
                    target: true_label.clone(),
                });
                let source_2 = convert_expression(*right, instructions);
                instructions.push([
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
                ]);
                result
            }

            ProcessedBinop::Normal(operator) => {
                let source_1 = convert_expression(*left, instructions);
                let source_2 = convert_expression(*right, instructions);
                let dst = Value::Var(new_var());
                let binary = Instruction::Binary {
                    operator,
                    source_1,
                    source_2,
                    dst: dst.clone(),
                };
                instructions.push_one(binary);
                dst
            }
            ProcessedBinop::Compound(op) => {
                let dst = convert_expression(*left, instructions);
                let modifier = convert_expression(*right, instructions);
                let binary = Instruction::Binary {
                    operator: op.into(),
                    source_1: dst.clone(),
                    source_2: modifier,
                    dst: dst.clone(),
                };
                instructions.push_one(binary);
                dst
            }
        },
        expression @ (AstExpression::Var(_)
        | AstExpression::Int(_)
        | AstExpression::Unary(_)
        | AstExpression::PrefixIncrement(_)
        | AstExpression::PrefixDecrement(_)
        | AstExpression::PostfixIncrement(_)
        | AstExpression::PostfixDecrement(_)
        | AstExpression::Nested(_)) => {
            let factor = AstFactor::try_from(expression).unwrap();
            convert_factor(factor, instructions)
        }

        AstExpression::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            let c = convert_expression(*condition, instructions);
            let result = Value::Var(new_var());
            let false_label = conditional_label();

            let end = conditional_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: false_label.clone(),
            });
            let true_res = convert_expression(*r#true, instructions);
            instructions.push([
                Instruction::Copy {
                    src: true_res,
                    dst: result.clone(),
                },
                Instruction::Jump {
                    target: end.clone(),
                },
                Instruction::Label(false_label),
            ]);
            let false_res = convert_expression(*r#false, instructions);
            instructions.push([
                Instruction::Copy {
                    src: false_res,
                    dst: result.clone(),
                },
                Instruction::Label(end),
            ]);
            result
        }
    }
}

enum ProcessedBinop {
    LogAnd,
    LogOr,
    Normal(TackyBinary),
    Compound(CompoundOp),
}

enum CompoundOp {
    Plus,
    Minus,
    Times,
    Div,
    Rem,
    And,
    Or,
    Xor,
    LeftShift,
    RightShift,
}

impl From<CompoundOp> for TackyBinary {
    fn from(other: CompoundOp) -> Self {
        match other {
            CompoundOp::Plus => Self::Add,
            CompoundOp::Minus => Self::Subtract,
            CompoundOp::Times => Self::Multiply,
            CompoundOp::Div => Self::Divide,
            CompoundOp::Rem => Self::Remainder,
            CompoundOp::And => Self::BitAnd,
            CompoundOp::Or => Self::BitOr,
            CompoundOp::Xor => Self::Xor,
            CompoundOp::LeftShift => Self::LeftShift,
            CompoundOp::RightShift => Self::RightShift,
        }
    }
}

const fn process_binop(binop: AstBinop) -> ProcessedBinop {
    use AstBinop as Pre;
    use ProcessedBinop as Post;
    match binop {
        Pre::Equals => unreachable!(),
        Pre::Ternary => unreachable!(),
        Pre::LogAnd => Post::LogAnd,
        Pre::LogOr => Post::LogOr,
        Pre::Add => Post::Normal(TackyBinary::Add),
        Pre::Subtract => Post::Normal(TackyBinary::Subtract),
        Pre::Multiply => Post::Normal(TackyBinary::Multiply),
        Pre::BitAnd => Post::Normal(TackyBinary::BitAnd),
        Pre::BitOr => Post::Normal(TackyBinary::BitOr),
        Pre::Xor => Post::Normal(TackyBinary::Xor),
        Pre::LeftShift => Post::Normal(TackyBinary::LeftShift),
        Pre::RightShift => Post::Normal(TackyBinary::RightShift),
        Pre::EqualTo => Post::Normal(TackyBinary::EqualTo),
        Pre::NotEqual => Post::Normal(TackyBinary::NotEqual),
        Pre::LessThan => Post::Normal(TackyBinary::LessThan),
        Pre::GreaterThan => Post::Normal(TackyBinary::GreaterThan),
        Pre::Leq => Post::Normal(TackyBinary::Leq),
        Pre::Geq => Post::Normal(TackyBinary::Geq),
        Pre::Divide => Post::Normal(TackyBinary::Divide),
        Pre::Remainder => Post::Normal(TackyBinary::Remainder),
        Pre::PlusEquals => Post::Compound(CompoundOp::Plus),
        Pre::MinusEquals => Post::Compound(CompoundOp::Minus),
        Pre::TimesEqual => Post::Compound(CompoundOp::Times),
        Pre::DivEqual => Post::Compound(CompoundOp::Div),

        Pre::RemEqual => Post::Compound(CompoundOp::Rem),
        Pre::BitAndEqual => Post::Compound(CompoundOp::And),
        Pre::BitOrEqual => Post::Compound(CompoundOp::Or),
        Pre::BitXorEqual => Post::Compound(CompoundOp::Xor),
        Pre::LeftShiftEqual => Post::Compound(CompoundOp::LeftShift),
        Pre::RightShiftEqual => Post::Compound(CompoundOp::RightShift),
    }
}
fn convert_factor(factor: AstFactor, instructions: &mut OpVec<Instruction>) -> Value {
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
            instructions.push_one(unary);
            Value::Var(tmp)
        }
        AstFactor::Nested(e) => convert_expression(*e, instructions),
        AstFactor::Var(v) => Value::Var(v.into()),
        AstFactor::PrefixIncrement(exp) => {
            //prefix
            let expression_result = convert_expression(*exp, instructions);
            let unary = Instruction::Binary {
                operator: TackyBinary::Add,
                source_1: expression_result.clone(),
                source_2: Value::Constant(1),
                dst: expression_result.clone(),
            };
            instructions.push_one(unary);
            expression_result
        }
        AstFactor::PrefixDecrement(exp) => {
            let expression_result = convert_expression(*exp, instructions);
            let unary = Instruction::Binary {
                operator: TackyBinary::Subtract,
                source_1: expression_result.clone(),
                source_2: Value::Constant(1),
                dst: expression_result.clone(),
            };
            instructions.push_one(unary);
            expression_result
        }
        AstFactor::PostfixDecrement(exp) => {
            let expression_result = convert_expression(*exp, instructions);
            let old_value_location = new_var();
            instructions.push([
                Instruction::Copy {
                    src: expression_result.clone(),
                    dst: Value::Var(old_value_location.clone()),
                },
                Instruction::Binary {
                    operator: TackyBinary::Subtract,
                    source_1: expression_result.clone(),
                    source_2: Value::Constant(1),
                    dst: expression_result,
                },
            ]);
            Value::Var(old_value_location)
        }

        AstFactor::PostfixIncrement(exp) => {
            let expression_result = convert_expression(*exp, instructions);
            let old_value_location = new_var();
            instructions.push([
                Instruction::Copy {
                    src: expression_result.clone(),
                    dst: Value::Var(old_value_location.clone()),
                },
                Instruction::Binary {
                    operator: TackyBinary::Add,
                    source_1: expression_result.clone(),
                    source_2: Value::Constant(1),
                    dst: expression_result,
                },
            ]);
            Value::Var(old_value_location)
        }
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

fn if_label() -> Rc<Identifier> {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("if{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}

fn else_label() -> Rc<Identifier> {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("else{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}

fn conditional_label() -> Rc<Identifier> {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("c{number}").into_bytes().into();
    Rc::new(Identifier(var_name))
}
