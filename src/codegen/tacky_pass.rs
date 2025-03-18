use super::assembly;
use super::Identifier;
use crate::lex::Constant;
use crate::parse;
use assembly::tacky::FunctionDefinition;
use assembly::tacky::Instruction;
use assembly::tacky::Program;
use assembly::tacky::TackyBinary;
use assembly::tacky::TopLevel;
use assembly::tacky::Value;
use assembly::OpVec;

use crate::semantics;
use assembly::tacky::StaticVar;
use parse::BinaryOperator as AstBinop;
use parse::VarType;
use semantics::Attr;
use semantics::Declaration as AstDeclaration;
use semantics::Expression as AstExpression;
use semantics::ForInit;

use semantics::Label as AstLabel;
use semantics::TypedProgram as AstProgram;

use parse::ParamList;
use parse::StorageClass;
use semantics::StatementLabels;

use semantics::Block as AstBlock;
use semantics::BlockItem as AstBlockItem;
use semantics::Statement as AstStatement;
use semantics::SymbolTable;
use semantics::TypedExp;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

static TEMP_COUNT: AtomicUsize = AtomicUsize::new(0);

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

const fn function_is_global(sc: &Option<StorageClass>) -> bool {
    matches!(sc, Some(StorageClass::Extern) | None)
}

pub fn emit(program: AstProgram, symbol_table: &mut SymbolTable) -> Program {
    let mut tlvs = Vec::with_capacity(program.0.len());
    for dec in program.0 {
        if let AstDeclaration::Function {
            name,
            params,
            body,
            storage_class,
            r#type,
        } = dec
        {
            if let Some(f) = convert_function(
                name,
                body,
                params,
                function_is_global(&storage_class),
                symbol_table,
            ) {
                tlvs.push(TopLevel::Fn(f));
            }
        }
    }

    tlvs.reverse();

    for (name, attr) in symbol_table {
        if let Attr::Static {
            init: Some(crate::semantics::typecheck::InitialVal::Initial(init)),
            global,
            r#type,
        } = attr
        {
            tlvs.push(TopLevel::StaticVar(StaticVar {
                name: name.clone(),
                global: *global,
                init: *init,
                typ: *r#type,
            }))
        }
    }

    tlvs.reverse();
    Program(tlvs.into_boxed_slice())
}

fn convert_function(
    name: Identifier,
    body: Option<Box<[AstBlockItem<TypedExp>]>>,
    params: ParamList,
    global: bool,
    table: &mut SymbolTable,
) -> Option<FunctionDefinition> {
    let mut body_ops = OpVec::new();
    convert_block(body?, &mut body_ops, &name, table);
    body_ops.push_one(Instruction::Return(Value::Constant(Constant::Long(0))));

    Some(FunctionDefinition {
        name: name.clone(),
        params: params.into_iter().map(|param| param.name).collect(),
        body: body_ops.into(),
        global,
    })
}

fn convert_block(
    block: AstBlock<TypedExp>,
    instructions: &mut OpVec<Instruction>,
    fn_name: &Identifier,
    table: &mut SymbolTable,
) {
    for block in block {
        match block {
            AstBlockItem::S(statement) => {
                convert_statement(statement, instructions, fn_name, table)
            }
            AstBlockItem::D(declaration) => convert_declaration(declaration, instructions, table),
        };
    }
}

fn convert_statement(
    statement: AstStatement<TypedExp>,
    instructions: &mut OpVec<Instruction>,
    fn_name: &Identifier,
    table: &mut SymbolTable,
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
            convert_statement(*body, instructions, fn_name, table);
            instructions.push_one(Instruction::Label(r#continue));
            let result = convert_expression(condition, instructions, table);
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
            let result = convert_expression(condition, instructions, table);
            instructions.push_one(Instruction::JumpIfZero {
                condition: result,
                target: r#break.clone(),
            });
            convert_statement(*body, instructions, fn_name, table);
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
                Some(ForInit::D {
                    name,
                    initializer,
                    r#type,
                    sc,
                }) => convert_declaration(
                    AstDeclaration::Var {
                        name,
                        init: initializer,
                        storage_class: sc,
                        r#type,
                    },
                    instructions,
                    table,
                ),
                Some(ForInit::E(exp)) => {
                    convert_expression(exp, instructions, table);
                }
                None => {}
            };
            instructions.push_one(Instruction::Label(start.clone()));
            if let Some(condition) = condition {
                let v = convert_expression(condition, instructions, table);
                instructions.push_one(Instruction::JumpIfZero {
                    condition: v,
                    target: r#break.clone(),
                })
            }
            convert_statement(*body, instructions, fn_name, table);
            //
            instructions.push_one(Instruction::Label(r#continue));
            if let Some(post) = post {
                convert_expression(post, instructions, table);
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

        AstStatement::Compound(block) => convert_block(block, instructions, fn_name, table),

        AstStatement::Ret(e) => {
            let result = convert_expression(e, instructions, table);
            instructions.push_one(Instruction::Return(result));
        }
        AstStatement::Null => {}
        AstStatement::Exp(e) => {
            let _ = convert_expression(e, instructions, table);
        }
        AstStatement::If {
            condition,
            then,
            r#else: None,
        } => {
            let c = convert_expression(condition, instructions, table);
            let label = if_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: label.clone(),
            });
            convert_statement(*then, instructions, fn_name, table);
            instructions.push_one(Instruction::Label(label));
        }
        AstStatement::If {
            condition,
            then,
            r#else: Some(r#else),
        } => {
            let c = convert_expression(condition, instructions, table);
            let else_label = else_label();
            let end = if_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: else_label.clone(),
            });
            convert_statement(*then, instructions, fn_name, table);
            instructions.push([
                Instruction::Jump {
                    target: end.clone(),
                },
                Instruction::Label(else_label),
            ]);
            convert_statement(*r#else, instructions, fn_name, table);
            instructions.push_one(Instruction::Label(end));
        }

        AstStatement::Label {
            name: AstLabel::Named(name),
            body,
        } => {
            instructions.push_one(Instruction::Label(named_label(&name, fn_name)));
            convert_statement(*body, instructions, fn_name, table);
        }
        AstStatement::Label {
            name: AstLabel::Case { val, id },
            body,
        } => {
            instructions.push_one(Instruction::Label(id.case(val)));
            convert_statement(*body, instructions, fn_name, table);
        }
        AstStatement::Label {
            name: AstLabel::Default(id),
            body,
        } => {
            instructions.push_one(Instruction::Label(id.default()));
            convert_statement(*body, instructions, fn_name, table);
        }
        AstStatement::Switch {
            val,
            label,
            cases,
            body,
            default,
        } => {
            let end_label = label.labels().r#break;
            let switch_val = convert_expression(val, instructions, table);
            for case in cases {
                let target_var = new_var(VarType::Int, table);
                instructions.push([
                    // if val == case
                    Instruction::Binary {
                        operator: TackyBinary::EqualTo,
                        source_1: switch_val.clone(),
                        source_2: Value::Constant(Constant::Integer(case)),
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
            convert_statement(*body, instructions, fn_name, table);
            instructions.push_one(Instruction::Label(end_label));
        }
        AstStatement::Goto(label) => instructions.push_one(Instruction::Jump {
            target: named_label(&label, fn_name),
        }),
    }
}

fn convert_declaration(
    dec: AstDeclaration<TypedExp>,
    instructions: &mut OpVec<Instruction>,
    table: &mut SymbolTable,
) {
    match dec {
        AstDeclaration::Function { body: None, .. } => (),

        AstDeclaration::Function { body: Some(_), .. } => unreachable!(),

        AstDeclaration::Var {
            name,
            init: Some(init),
            storage_class: None | Some(StorageClass::Extern),
            r#type,
        } => {
            let result = convert_expression(init, instructions, table);
            instructions.push_one(Instruction::Copy {
                src: result,
                dst: Value::Var(name),
            })
        }

        AstDeclaration::Var { .. } => {}
    }
}

fn convert_expression(
    TypedExp { r#type: typ, exp }: TypedExp,
    instructions: &mut OpVec<Instruction>,
    table: &mut SymbolTable,
) -> Value {
    match *exp {
        AstExpression::Cast { target, exp } if target == typ => {
            convert_expression(exp, instructions, table)
        }

        AstExpression::Cast { target, exp } => {
            let res = convert_expression(exp, instructions, table);
            let dst = Value::Var(new_var(typ, r#table));
            // add dst to symbol table
            //
            instructions.push_one(match target {
                // long to int
                VarType::Int => Instruction::Truncate {
                    src: res,
                    dst: dst.clone(),
                },
                VarType::Long => Instruction::SignExtend {
                    src: res,
                    dst: dst.clone(),
                },
            });

            dst
        }
        AstExpression::Const(c) => Value::Constant(c),
        AstExpression::FunctionCall { name, args } => {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push(convert_expression(arg, instructions, table));
            }
            let result = Value::Var(new_var(VarType::Int, table));
            instructions.push_one(Instruction::FunCall {
                name,
                args: args_vec.into(),
                dst: result.clone(),
            });
            result
        }
        AstExpression::Assignment { from: var, to: rhs } => {
            let AstExpression::Var(var) = *var.exp else {
                unreachable!()
            };
            let result = convert_expression(rhs, instructions, table);
            let var = Value::Var(var);
            instructions.push_one(Instruction::Copy {
                src: result,
                dst: var.clone(),
            });
            var
        }
        AstExpression::Binary {
            operator,
            left,
            right,
        } => match process_binop(operator) {
            ProcessedBinop::LogAnd => {
                let source_1 = convert_expression(left, instructions, table);
                let false_label = and_label();
                let end_label = end_label();
                let result = Value::Var(new_var(typ, table));
                instructions.push_one(Instruction::JumpIfZero {
                    condition: source_1,
                    target: false_label.clone(),
                });
                let source_2 = convert_expression(right, instructions, table);
                instructions.push([
                    Instruction::JumpIfZero {
                        condition: source_2,
                        target: false_label.clone(),
                    },
                    Instruction::Copy {
                        src: Value::Constant(Constant::Integer(1)),
                        dst: result.clone(),
                    },
                    Instruction::Jump {
                        target: end_label.clone(),
                    },
                    Instruction::Label(false_label),
                    Instruction::Copy {
                        src: Value::Constant(Constant::Integer(0)),
                        dst: result.clone(),
                    },
                    Instruction::Label(end_label.clone()),
                ]);
                result
            }
            ProcessedBinop::LogOr => {
                let source_1 = convert_expression(left, instructions, table);
                let true_label = or_label();
                let end_label = end_label();
                let result = Value::Var(new_var(typ, table));
                // if source 1 is true we jump to true label
                instructions.push_one(Instruction::JumpIfNotZero {
                    condition: source_1,
                    target: true_label.clone(),
                });
                let source_2 = convert_expression(right, instructions, table);
                instructions.push([
                    Instruction::JumpIfNotZero {
                        condition: source_2,
                        target: true_label.clone(),
                    },
                    Instruction::Copy {
                        src: Value::Constant(Constant::Integer(0)),
                        dst: result.clone(),
                    },
                    Instruction::Jump {
                        target: end_label.clone(),
                    },
                    Instruction::Label(true_label),
                    Instruction::Copy {
                        src: Value::Constant(Constant::Integer(1)),
                        dst: result.clone(),
                    },
                    Instruction::Label(end_label),
                ]);
                result
            }

            ProcessedBinop::Normal(operator) => {
                let source_1 = convert_expression(left, instructions, table);
                let source_2 = convert_expression(right, instructions, table);
                let dst = Value::Var(new_var(typ, table));
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
                let dst = convert_expression(left, instructions, table);
                let modifier = convert_expression(right, instructions, table);
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

        //AstExpression::Nested(e) => {}
        AstExpression::Conditional {
            condition,
            r#true,
            r#false,
        } => {
            let c = convert_expression(condition, instructions, table);
            let result = Value::Var(new_var(typ, table));
            let false_label = conditional_label();

            let end = conditional_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: false_label.clone(),
            });
            let true_res = convert_expression(r#true, instructions, table);
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
            let false_res = convert_expression(r#false, instructions, table);
            instructions.push([
                Instruction::Copy {
                    src: false_res,
                    dst: result.clone(),
                },
                Instruction::Label(end),
            ]);
            result
        }
        AstExpression::Unary { operator, operand } => {
            let tmp = new_var(typ, table);
            let factor_result = convert_expression(operand, instructions, table);
            let unary = Instruction::Unary {
                op: operator,
                source: factor_result,
                dst: tmp.clone(),
            };
            instructions.push_one(unary);
            Value::Var(tmp)
        }
        AstExpression::Nested(e) => convert_expression(e, instructions, table),
        AstExpression::Var(v) => Value::Var(v),
        AstExpression::PrefixIncrement(exp) => {
            //prefix
            let expression_result = convert_expression(exp, instructions, table);
            let unary = Instruction::Binary {
                operator: TackyBinary::Add,
                source_1: expression_result.clone(),
                source_2: Value::Constant(Constant::Integer(1)),
                dst: expression_result.clone(),
            };
            instructions.push_one(unary);
            expression_result
        }
        AstExpression::PrefixDecrement(exp) => {
            let expression_result = convert_expression(exp, instructions, table);
            let unary = Instruction::Binary {
                operator: TackyBinary::Subtract,
                source_1: expression_result.clone(),
                source_2: Value::Constant(Constant::Integer(1)),
                dst: expression_result.clone(),
            };
            instructions.push_one(unary);
            expression_result
        }
        AstExpression::PostfixDecrement(exp) => {
            let expression_result = convert_expression(exp, instructions, table);
            let old_value_location = new_var(typ, table);
            instructions.push([
                Instruction::Copy {
                    src: expression_result.clone(),
                    dst: Value::Var(old_value_location.clone()),
                },
                Instruction::Binary {
                    operator: TackyBinary::Subtract,
                    source_1: expression_result.clone(),
                    source_2: Value::Constant(Constant::Integer(1)),
                    dst: expression_result,
                },
            ]);
            Value::Var(old_value_location)
        }
        AstExpression::PostfixIncrement(exp) => {
            let expression_result = convert_expression(exp, instructions, table);
            let old_value_location = new_var(typ, table);
            instructions.push([
                Instruction::Copy {
                    src: expression_result.clone(),
                    dst: Value::Var(old_value_location.clone()),
                },
                Instruction::Binary {
                    operator: TackyBinary::Add,
                    source_1: expression_result.clone(),
                    source_2: Value::Constant(Constant::Integer(1)),
                    dst: expression_result,
                },
            ]);
            Value::Var(old_value_location)
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

fn new_var(typ: VarType, symbols: &mut SymbolTable) -> Identifier {
    let number = TEMP_COUNT.fetch_add(1, Ordering::SeqCst);
    let var_name: Box<[u8]> = format!("tmp_{number}").into_bytes().into();
    let ident = Identifier(var_name.into());
    symbols.insert(ident.clone(), Attr::Automatic(typ));
    ident
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
