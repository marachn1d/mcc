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
use parse::inc_dec::*;
use parse::VarType;
use semantics::typed::{
    self, Block, BlockItem, Dec, Expr, Fix, FnDec, ForInit, Label, Stmnt, VarDec,
};
use semantics::Attr;

use parse::StorageClass;
use semantics::StatementLabels;

use semantics::SymbolTable;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

static TEMP_COUNT: AtomicUsize = AtomicUsize::new(0);

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

pub fn emit(program: typed::Program, symbol_table: &mut SymbolTable) -> Program {
    let mut tlvs = Vec::with_capacity(program.len());
    for dec in program {
        if let Dec::Fn(f) = dec {
            if let Some(f) = convert_function(f, symbol_table) {
                tlvs.push(TopLevel::Fn(f));
            }
        }
    }

    tlvs.reverse();

    for (name, attr) in symbol_table {
        if let Attr::Static {
            init: Some(init),
            global: g,
            typ: t,
        } = attr
        {
            let init = init.get_static(*t);
            tlvs.push(TopLevel::StaticVar(StaticVar {
                name: name.clone(),
                global: *g,
                init,
                typ: *t,
            }))
        }
    }

    tlvs.reverse();
    Program(tlvs.into_boxed_slice())
}

fn convert_function(
    FnDec {
        name,
        params,
        body,
        typ: _,
        sc,
    }: FnDec,
    table: &mut SymbolTable,
) -> Option<FunctionDefinition> {
    let mut body_ops = OpVec::new();
    convert_block(body?, &mut body_ops, &name, table);
    body_ops.push_one(Instruction::Return(Value::Constant(Constant::Long(0))));

    Some(FunctionDefinition {
        name: name.clone(),
        params: params.into_iter().map(|param| param.name).collect(),
        body: body_ops.into(),
        global: sc.is_none_or(|sc| sc == StorageClass::Extern),
    })
}

fn convert_block(
    block: Block,
    instructions: &mut OpVec<Instruction>,
    fn_name: &Identifier,
    table: &mut SymbolTable,
) {
    for block in block {
        match block {
            BlockItem::S(statement) => convert_statement(statement, instructions, fn_name, table),
            BlockItem::D(Dec::Var(v)) => convert_vardec(v, instructions, table),
            _ => (),
        };
    }
}

fn convert_statement(
    statement: Stmnt,
    instructions: &mut OpVec<Instruction>,
    fn_name: &Identifier,
    table: &mut SymbolTable,
) {
    match statement {
        Stmnt::DoWhile {
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
        Stmnt::While {
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
        Stmnt::For {
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

            match init.map(|x| *x) {
                Some(ForInit::D(v)) => convert_vardec(v, instructions, table),
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
        Stmnt::Break(label) => {
            instructions.push_one(Instruction::Jump {
                target: label.r#break(),
            });
        }
        Stmnt::Continue(label) => {
            instructions.push_one(Instruction::Jump {
                target: label.r#continue(),
            });
        }

        Stmnt::Compound(block) => convert_block(block, instructions, fn_name, table),

        Stmnt::Ret(e) => {
            let result = convert_expression(e, instructions, table);
            instructions.push_one(Instruction::Return(result));
        }
        Stmnt::Null => {}
        Stmnt::Exp(e) => {
            let _ = convert_expression(e, instructions, table);
        }
        Stmnt::If {
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
        Stmnt::If {
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

        Stmnt::Label {
            name: Label::Named(name),
            body,
        } => {
            instructions.push_one(Instruction::Label(named_label(&name, fn_name)));
            convert_statement(*body, instructions, fn_name, table);
        }
        Stmnt::Label {
            name: Label::Case { c, id },
            body,
        } => {
            instructions.push_one(Instruction::Label(id.case(c)));
            convert_statement(*body, instructions, fn_name, table);
        }
        Stmnt::Label {
            name: Label::Default(id),
            body,
        } => {
            instructions.push_one(Instruction::Label(id.default()));
            convert_statement(*body, instructions, fn_name, table);
        }
        Stmnt::Switch {
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
            convert_statement(*body, instructions, fn_name, table);
            instructions.push_one(Instruction::Label(end_label));
        }
        Stmnt::Goto(label) => instructions.push_one(Instruction::Jump {
            target: named_label(&label, fn_name),
        }),
    }
}

fn convert_cast_op(
    ty: VarType,
    src: Value,
    d: Value,
    instructions: &mut OpVec<Instruction>,
) -> Value {
    let dst = d.clone();
    let op = match ty {
        VarType::Int => Instruction::Truncate { src, dst },
        VarType::Long => Instruction::SignExtend { src, dst },
    };
    instructions.push_one(op);
    d
}

fn convert_cast(
    (target, exp, ty): (VarType, Expr, VarType),
    instructions: &mut OpVec<Instruction>,
    table: &mut SymbolTable,
) -> Value {
    match (target, exp, ty) {
        (target, exp, _) if target == exp.ty() => convert_expression(exp, instructions, table),
        (_, exp, ty) => {
            let src = convert_expression(exp, instructions, table);
            let dst = Value::Var(new_var(ty, r#table));
            // add dst to symbol table
            //
            convert_cast_op(ty, src, dst, instructions)
        }
    }
}

fn convert_vardec(dec: VarDec, instructions: &mut OpVec<Instruction>, table: &mut SymbolTable) {
    if let VarDec {
        name,
        init: Some(init),
        sc: None | Some(StorageClass::Extern),
        typ: _,
    } = dec
    {
        let result = convert_expression(init, instructions, table);
        instructions.push_one(Instruction::Copy {
            src: result,
            dst: Value::Var(name),
        })
    }
}

fn convert_expression(
    exp: Expr,
    instructions: &mut OpVec<Instruction>,
    table: &mut SymbolTable,
) -> Value {
    match exp {
        Expr::Cast { target, exp, ty } => convert_cast((target, *exp, ty), instructions, table),

        Expr::Const { cnst: c, ty: _ } => Value::Constant(c),
        Expr::FunctionCall { name, args, ty } => {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push(convert_expression(arg, instructions, table));
            }
            let result = Value::Var(new_var(ty, table));
            instructions.push_one(Instruction::FunCall {
                name,
                args: args_vec.into(),
                dst: result.clone(),
            });
            result
        }
        Expr::Assignment { dst, src, ty: _ } => {
            let Expr::Var { name: v, .. } = *dst else {
                unreachable!()
            };
            let result = convert_expression(*src, instructions, table);
            let var = Value::Var(v);
            instructions.push_one(Instruction::Copy {
                src: result,
                dst: var.clone(),
            });
            var
        }
        Expr::Binary {
            operator,
            left,
            right,
            ty,
        } => match process_binop(operator) {
            ProcessedBinop::LogAnd => {
                let source_1 = convert_expression(*left, instructions, table);
                let false_label = and_label();
                let end_label = end_label();
                let result = Value::Var(new_var(ty, table));
                instructions.push_one(Instruction::JumpIfZero {
                    condition: source_1,
                    target: false_label.clone(),
                });
                let source_2 = convert_expression(*right, instructions, table);
                instructions.push([
                    Instruction::JumpIfZero {
                        condition: source_2,
                        target: false_label.clone(),
                    },
                    Instruction::Copy {
                        src: Value::Constant(Constant::Int(1)),
                        dst: result.clone(),
                    },
                    Instruction::Jump {
                        target: end_label.clone(),
                    },
                    Instruction::Label(false_label),
                    Instruction::Copy {
                        src: Value::Constant(Constant::Int(0)),
                        dst: result.clone(),
                    },
                    Instruction::Label(end_label.clone()),
                ]);
                result
            }
            ProcessedBinop::LogOr => {
                let source_1 = convert_expression(*left, instructions, table);
                let true_label = or_label();
                let end_label = end_label();
                let result = Value::Var(new_var(ty, table));
                // if source 1 is true we jump to true label
                instructions.push_one(Instruction::JumpIfNotZero {
                    condition: source_1,
                    target: true_label.clone(),
                });
                let source_2 = convert_expression(*right, instructions, table);
                instructions.push([
                    Instruction::JumpIfNotZero {
                        condition: source_2,
                        target: true_label.clone(),
                    },
                    Instruction::Copy {
                        src: Value::Constant(Constant::Int(0)),
                        dst: result.clone(),
                    },
                    Instruction::Jump {
                        target: end_label.clone(),
                    },
                    Instruction::Label(true_label),
                    Instruction::Copy {
                        src: Value::Constant(Constant::Int(1)),
                        dst: result.clone(),
                    },
                    Instruction::Label(end_label),
                ]);
                result
            }

            ProcessedBinop::Normal(operator) => {
                let source_1 = convert_expression(*left, instructions, table);
                let source_2 = convert_expression(*right, instructions, table);
                let dst = Value::Var(new_var(ty, table));
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
                let dst = convert_expression(*left, instructions, table);
                let modifier = convert_expression(*right, instructions, table);
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

        Expr::Conditional {
            condition,
            r#true,
            r#false,
            ty,
        } => {
            let c = convert_expression(*condition, instructions, table);
            let result = Value::Var(new_var(ty, table));
            let false_label = conditional_label();

            let end = conditional_label();
            instructions.push_one(Instruction::JumpIfZero {
                condition: c,
                target: false_label.clone(),
            });
            let true_res = convert_expression(*r#true, instructions, table);
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
            let false_res = convert_expression(*r#false, instructions, table);
            instructions.push([
                Instruction::Copy {
                    src: false_res,
                    dst: result.clone(),
                },
                Instruction::Label(end),
            ]);
            result
        }
        Expr::Unary {
            operator,
            operand,
            ty,
        } => {
            let tmp = new_var(ty, table);
            let factor_result = convert_expression(*operand, instructions, table);
            let unary = Instruction::Unary {
                op: operator,
                source: factor_result,
                dst: tmp.clone(),
            };
            instructions.push_one(unary);
            Value::Var(tmp)
        }
        Expr::Nested { inner: e, .. } => convert_expression(*e, instructions, table),
        Expr::Var { name: v, .. } => Value::Var(v),
        Expr::IncDec {
            op: IncDec { inc, fix: Fix::Pre },
            ty: _,
            exp,
        } => {
            let expression_result = convert_expression(*exp, instructions, table);
            let op = match inc {
                IncOp::Inc => TackyBinary::Add,
                IncOp::Dec => TackyBinary::Subtract,
            };

            //prefix
            instructions.push_one(Instruction::Binary {
                operator: op,
                source_1: expression_result.clone(),
                source_2: Value::Constant(Constant::Int(1)),
                dst: expression_result.clone(),
            });
            expression_result
        }
        Expr::IncDec {
            op: IncDec {
                inc,
                fix: Fix::Post,
            },
            ty,
            exp,
        } => {
            let res = convert_expression(*exp, instructions, table);
            let op = match inc {
                IncOp::Inc => TackyBinary::Add,
                IncOp::Dec => TackyBinary::Subtract,
            };

            let old_val = Value::Var(new_var(ty, table));
            //prefix
            instructions.push([
                Instruction::Copy {
                    src: res.clone(),
                    dst: old_val.clone(),
                },
                Instruction::Binary {
                    operator: op,
                    source_1: res.clone(),
                    source_2: Value::Constant(Constant::Int(1)),
                    dst: res.clone(),
                },
            ]);
            old_val
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

const fn process_binop(binop: parse::Bop) -> ProcessedBinop {
    use parse::Bop as Pre;
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
