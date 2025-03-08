use super::assembly;
use super::Identifier;
use crate::parse;
use assembly::tacky::FunctionDefinition;
use assembly::tacky::Instruction;
use assembly::tacky::Program;
use assembly::tacky::TackyBinary;
use assembly::tacky::Value;
use assembly::OpVec;
use assembly::TopLevel;
use parse::Binary as AstBinary;

use crate::semantics;
use assembly::StaticVar;
use parse::BinaryOperator as AstBinop;
use parse::Expression as AstExpression;
use parse::Factor as AstFactor;
use parse::ForInit;
use parse::Unary as AstUnary;
use semantics::Attr;
use semantics::Declaration as AstDeclaration;

use semantics::Label as AstLabel;
use semantics::Program as AstProgram;

use parse::ParamList;
use parse::StorageClass;
use semantics::StatementLabels;

use semantics::Block as AstBlock;
use semantics::BlockItem as AstBlockItem;
use semantics::Statement as AstStatement;
use semantics::SymbolTable;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
static TEMP_COUNT: AtomicUsize = AtomicUsize::new(0);

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

const fn function_is_global(sc: &Option<StorageClass>) -> bool {
    if let Some(StorageClass::Static) = sc {
        false
    } else {
        true
    }
}

pub fn emit(program: AstProgram, symbol_table: &SymbolTable) -> Program {
    let mut tlvs = Vec::with_capacity(program.0.len());
    for dec in program.0 {
        if let AstDeclaration::Function {
            name,
            params,
            body,
            storage_class,
        } = dec
        {
            if let Some(f) =
                convert_function(name, body, params, function_is_global(&storage_class))
            {
                tlvs.push(TopLevel::Fn(f));
            }
        }
    }

    for (name, attr) in symbol_table {
        if let Attr::StaticInt {
            init: Some(init),
            global,
        } = attr
        {
            tlvs.push(TopLevel::StaticVar(StaticVar {
                name: name.clone(),
                global: *global,
                init: init.as_num(),
            }))
        }
    }

    super::Program::<Instruction>(tlvs.into_boxed_slice())
}

fn convert_function(
    name: Identifier,
    body: Option<Box<[AstBlockItem]>>,
    params: ParamList,
    global: bool,
) -> Option<FunctionDefinition> {
    let mut body_ops = OpVec::new();
    convert_block(body?, &mut body_ops, &name);
    body_ops.push_one(Instruction::Return(Value::Constant(0)));

    Some(FunctionDefinition {
        name: name.clone(),
        params: match params {
            ParamList::Void => [].into(),

            ParamList::Int(names) => names,
        },
        body: body_ops.into(),
        global,
    })
}

fn convert_block(block: AstBlock, instructions: &mut OpVec<Instruction>, fn_name: &Identifier) {
    for block in block {
        match block {
            AstBlockItem::S(statement) => convert_statement(statement, instructions, fn_name),
            AstBlockItem::D(declaration) => convert_declaration(declaration, instructions),
        };
    }
}

fn convert_statement(
    statement: AstStatement,
    instructions: &mut OpVec<Instruction>,
    fn_name: &Identifier,
) {
    match statement {
        AstStatement::DoWhile {
            body,
            condition,
            label,
        } => {
            let StatementLabels {
                start,
                r#break,
                r#continue,
                end: _,
            } = label.labels();
            instructions.push_one(Instruction::Label(start.clone()));
            convert_statement(*body, instructions, fn_name);
            instructions.push_one(Instruction::Label(r#continue));
            let result = convert_expression(condition, instructions);
            instructions.push([
                Instruction::JumpIfNotZero {
                    condition: result,
                    target: start,
                },
                Instruction::Label(r#break),
            ]);
        }
        AstStatement::While {
            body,
            condition,
            label,
        } => {
            let StatementLabels {
                start: _,
                r#break,
                r#continue,
                end: _,
            } = label.labels();

            instructions.push_one(Instruction::Label(r#continue.clone()));
            let result = convert_expression(condition, instructions);
            instructions.push_one(Instruction::JumpIfZero {
                condition: result,
                target: r#break.clone(),
            });
            convert_statement(*body, instructions, fn_name);
            instructions.push([
                Instruction::Jump { target: r#continue },
                Instruction::Label(r#break),
            ]);
        }
        AstStatement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let StatementLabels {
                start,
                r#break,
                r#continue,
                end: _,
            } = label.labels();

            match init {
                Some(ForInit::D(d)) => convert_declaration(d.into(), instructions),
                Some(ForInit::E(exp)) => {
                    convert_expression(exp, instructions);
                }
                None => {}
            };
            instructions.push_one(Instruction::Label(start.clone()));
            if let Some(condition) = condition {
                let v = convert_expression(condition, instructions);
                instructions.push_one(Instruction::JumpIfZero {
                    condition: v,
                    target: r#break.clone(),
                })
            }
            convert_statement(*body, instructions, fn_name);
            //
            instructions.push_one(Instruction::Label(r#continue));
            if let Some(post) = post {
                convert_expression(post, instructions);
            }
            instructions.push([
                Instruction::Jump { target: start },
                Instruction::Label(r#break),
            ])
        }
        AstStatement::Break(label) => {
            instructions.push_one(Instruction::Jump {
                target: label.r#break(),
            });
        }
        AstStatement::Continue(label) => {
            instructions.push_one(Instruction::Jump {
                target: label.r#continue(),
            });
        }

        AstStatement::Compound(block) => convert_block(block, instructions, fn_name),

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
            convert_statement(*then, instructions, fn_name);
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
            convert_statement(*then, instructions, fn_name);
            instructions.push([
                Instruction::Jump {
                    target: end.clone(),
                },
                Instruction::Label(else_label),
            ]);
            convert_statement(*r#else, instructions, fn_name);
            instructions.push_one(Instruction::Label(end));
        }

        AstStatement::Label {
            name: AstLabel::Named(name),
            body,
        } => {
            instructions.push_one(Instruction::Label(named_label(&name, fn_name)));
            convert_statement(*body, instructions, fn_name);
        }
        AstStatement::Label {
            name: AstLabel::Case { val, id },
            body,
        } => {
            instructions.push_one(Instruction::Label(id.case(val)));
            convert_statement(*body, instructions, fn_name);
        }
        AstStatement::Label {
            name: AstLabel::Default(id),
            body,
        } => {
            instructions.push_one(Instruction::Label(id.default()));
            convert_statement(*body, instructions, fn_name);
        }
        AstStatement::Switch {
            val,
            label,
            cases,
            body,
            default,
        } => {
            let end_label = label.labels().r#break;
            let switch_val = convert_expression(val, instructions);
            for case in cases {
                let target_var = new_var();
                instructions.push([
                    // if val == case
                    Instruction::Binary {
                        operator: TackyBinary::EqualTo,
                        source_1: switch_val.clone(),
                        source_2: Value::Constant(case),
                        dst: Value::Var(target_var.clone()),
                    },
                    // jump to its label
                    Instruction::JumpIfNotZero {
                        condition: Value::Var(target_var),
                        target: label.case(case),
                    },
                ]);
            }
            instructions.push_one(Instruction::Jump {
                target: if default {
                    label.default()
                } else {
                    end_label.clone()
                },
            });
            convert_statement(*body, instructions, fn_name);
            instructions.push_one(Instruction::Label(end_label));
        }
        AstStatement::Goto(label) => instructions.push_one(Instruction::Jump {
            target: named_label(&label, fn_name),
        }),
    }
}

fn convert_declaration(dec: AstDeclaration, instructions: &mut OpVec<Instruction>) {
    match dec {
        AstDeclaration::Function {
            name: _,
            params: _,
            body: None,
            storage_class: _,
        } => {}

        AstDeclaration::Function {
            name: _,
            params: _,
            body: Some(_),
            storage_class: _,
        } => {
            unreachable!();
        }

        AstDeclaration::Var {
            name,
            init,
            storage_class: _,
        } => {
            if let Some(init) = init {
                let result = convert_expression(init, instructions);
                instructions.push_one(Instruction::Copy {
                    src: result,
                    dst: Value::Var(name),
                })
            }
        }
    }
}

fn convert_expression(expression: AstExpression, instructions: &mut OpVec<Instruction>) -> Value {
    match expression {
        AstExpression::FunctionCall { name, args } => {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push(convert_expression(arg, instructions));
            }
            let result = Value::Var(new_var());
            instructions.push_one(Instruction::FunCall {
                name,
                args: args_vec.into(),
                dst: result.clone(),
            });
            result
        }
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
        AstFactor::Var(v) => Value::Var(v),
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
        AstFactor::FunctionCall { name, args } => {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push(convert_expression(arg, instructions));
            }
            let result = Value::Var(new_var());
            instructions.push_one(Instruction::FunCall {
                name,
                args: args_vec.into(),
                dst: result.clone(),
            });
            result
        }
    }
}

fn new_var() -> Identifier {
    let number = TEMP_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("tmp_{number}").into_bytes().into();
    Identifier(var_name.into())
}

fn and_label() -> Identifier {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("and_false{number}").into_bytes().into();
    Identifier(var_name.into())
}

fn or_label() -> Identifier {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("or_true{number}").into_bytes().into();
    Identifier(var_name.into())
}

fn end_label() -> Identifier {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("end{number}").into_bytes().into();
    Identifier(var_name.into())
}

fn if_label() -> Identifier {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("if{number}").into_bytes().into();
    Identifier(var_name.into())
}

fn named_label(function: &Identifier, label: &Identifier) -> Identifier {
    let var_name: Box<[u8]> = format!("{function}f_{label}n").into_bytes().into();
    Identifier(var_name.into())
}

fn else_label() -> Identifier {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("else{number}").into_bytes().into();
    Identifier(var_name.into())
}

fn conditional_label() -> Identifier {
    let number = LABEL_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("c{number}").into_bytes().into();
    Identifier(var_name.into())
}
