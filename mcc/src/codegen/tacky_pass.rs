use crate::semantics::Attr;
use asm::tacky::*;
//use ast::labels::Label;
use super::SymKey;
use ast::typed;
use ast::types_prelude::VarType;
//use ast::AstLabel;
use super::SymbolTable;
use asm::tacky::TackyTmp;
use ast::Constant;
use ast::Key;
use ast::StorageClass;
use ast::{Fix, IncDec, IncOp};
use std::sync::atomic::AtomicUsize;

static TEMP_COUNT: AtomicUsize = AtomicUsize::new(0);

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

pub fn emit<'a>(program: &typed::Program<'a>, symbol_table: &mut SymbolTable<'a>) -> Program<'a> {
    let mut tlvs = Vec::with_capacity(program.len());
    for dec in program {
        if let typed::Dec::Fn(f) = dec {
            if let Some(f) = convert_function(f, symbol_table) {
                tlvs.push(TopLevel::Fn(f));
            }
        }
    }

    tlvs.reverse();

    for (name, attr) in symbol_table {
        let SymKey::Key(name) = name else {
            continue;
        };
        let Attr::Static {
            init: Some(init),
            global: g,
            typ: t,
        } = attr
        else {
            continue;
        };
        let init = init.get_static(*t);
        tlvs.push(TopLevel::StaticVar(StaticVar {
            name: *name,
            global: *g,
            init,
            typ: *t,
        }))
    }

    tlvs.reverse();
    Program(tlvs.into_boxed_slice())
}

fn convert_function<'a>(
    typed::FnDec {
        name,
        params,
        body,
        typ: _,
        sc,
    }: &typed::FnDec<'a>,
    table: &mut SymbolTable<'a>,
) -> Option<FnDef<'a>> {
    if let Some(body) = body {
        let mut body_ops = Vec::new();
        convert_block(body, &mut body_ops, name, table);
        body_ops.push(Op::Return(Val::Constant(Constant::Long(0))));

        Some(FnDef {
            name: *name,
            params: params.into_iter().map(|param| param.name).collect(),
            body: body_ops.into(),
            global: sc.is_none_or(|sc| sc == StorageClass::Extern),
        })
    } else {
        None
    }
}

fn convert_block<'a>(
    block: &[typed::BlockItem<'a>],
    ops: &mut Vec<Op<'a>>,
    fn_name: &Key<'a>,
    table: &mut SymbolTable<'a>,
) {
    for block in block {
        match block {
            typed::BlockItem::S(statement) => convert_statement(statement, ops, fn_name, table),
            typed::BlockItem::D(typed::Dec::Var(v)) => convert_vardec(v, ops, table),
            _ => (),
        };
    }
}

fn push_label(label: ast::Label, ops: &mut Vec<Op>) {
    ops.push(Op::Label(Label::Anon(label)))
}

fn convert_statement<'a>(
    statement: &typed::Stmnt<'a>,
    ops: &mut Vec<Op<'a>>,
    fn_name: &Key<'a>,
    table: &mut SymbolTable<'a>,
) {
    use typed::Stmnt::*;
    match statement {
        DoWhile {
            body,
            condition,
            label,
        } => {
            push_label(label.start(), ops);
            convert_statement(body, ops, fn_name, table);

            push_label(label.r#continue(), ops);
            let result = convert_expression(condition, ops, table);
            ops.extend([
                Op::JumpIfNotZero {
                    condition: result,
                    target: Label::start(label),
                },
                Op::Label(Label::break_(label)),
            ]);
        }
        While {
            body,
            condition,
            label: id,
        } => {
            push_label(id.r#continue(), ops);
            let result = convert_expression(condition, ops, table);
            ops.push(Op::JumpIfZero {
                condition: result,
                target: Label::break_(id),
            });
            convert_statement(body, ops, fn_name, table);
            ops.extend([
                Op::Jump {
                    target: Label::continue_(id),
                },
                Op::Label(Label::break_(id)),
            ]);
        }
        For {
            init,
            condition,
            post,
            body,
            label: id,
        } => {
            match init.as_ref().map(|x| x.as_ref()) {
                Some(typed::ForInit::D(v)) => convert_vardec(v, ops, table),
                Some(typed::ForInit::E(exp)) => {
                    convert_expression(exp, ops, table);
                }
                None => {}
            };
            ops.push(Op::Label(Label::start(id)));
            if let Some(condition) = condition {
                let v = convert_expression(condition, ops, table);
                ops.push(Op::JumpIfZero {
                    condition: v,
                    target: Label::break_(id),
                })
            }
            convert_statement(body, ops, fn_name, table);
            //
            ops.push(Op::Label(Label::continue_(id)));
            if let Some(post) = post {
                convert_expression(post, ops, table);
            }
            ops.extend([
                Op::Jump {
                    target: Label::start(id),
                },
                Op::Label(Label::break_(id)),
            ])
        }
        Break(id) => {
            ops.push(Op::Jump {
                target: Label::break_(id),
            });
        }
        Continue(id) => {
            ops.push(Op::Jump {
                target: Label::continue_(id),
            });
        }

        Compound(block) => convert_block(block, ops, fn_name, table),

        Ret(e) => {
            let result = convert_expression(e, ops, table);
            ops.push(Op::Return(result));
        }
        Null => {}
        Exp(e) => {
            let _ = convert_expression(e, ops, table);
        }
        If {
            condition,
            then,
            r#else: None,
        } => {
            let c = convert_expression(condition, ops, table);
            let label = Label::and();
            ops.push(Op::JumpIfZero {
                condition: c,
                //erm
                target: label,
            });
            convert_statement(then, ops, fn_name, table);
            ops.push(Op::Label(label));
        }
        If {
            condition,
            then,
            r#else: Some(r#else),
        } => {
            let c = convert_expression(condition, ops, table);
            let else_label = Label::else_();
            let end = Label::if_();
            ops.push(Op::JumpIfZero {
                condition: c,
                target: else_label.clone(),
            });
            convert_statement(then, ops, fn_name, table);
            ops.extend([Op::Jump { target: end }, Op::Label(else_label)]);
            convert_statement(r#else, ops, fn_name, table);
            ops.push(Op::Label(end));
        }

        NamedLabel { l, body } => {
            ops.push(Op::Label(Label::Named {
                lbl: *l,
                f: *fn_name,
            }));
            convert_statement(body, ops, fn_name, table);
        }
        Case {
            case,
            stmnt: ast::typed::LabelStmnt { label, body },
        } => {
            ops.push(Op::Label(Label::case(label, case)));
            convert_statement(body, ops, fn_name, table);
        }
        Default(ast::typed::LabelStmnt { label, body }) => {
            ops.push(Op::Label(Label::default(label)));
            convert_statement(body, ops, fn_name, table);
        }
        Switch {
            val,
            label,
            cases,
            body,
            default,
        } => {
            let end_label = Label::break_(label);
            let switch_val = convert_expression(val, ops, table);
            for case in cases {
                let target_var = new_var(VarType::Int, table);
                ops.extend([
                    // if val == case
                    Op::Binary {
                        operator: Bin::EqualTo,
                        source_1: switch_val.clone(),
                        source_2: Val::Constant(*case),
                        dst: target_var,
                    },
                    // jump to its label
                    Op::JumpIfNotZero {
                        condition: target_var,
                        target: Label::case(label, case),
                    },
                ]);
            }
            ops.push(Op::Jump {
                target: if *default {
                    Label::default(label)
                } else {
                    end_label.clone()
                },
            });
            convert_statement(body, ops, fn_name, table);
            ops.push(Op::Label(end_label));
        }
        Goto(l) => ops.push(Op::Jump {
            target: Label::Named {
                lbl: *l,
                f: *fn_name,
            },
        }),
    }
}

fn convert_cast_op<'a>(ty: VarType, src: Val<'a>, d: Val<'a>, ops: &mut Vec<Op<'a>>) -> Val<'a> {
    let dst = d.clone();
    let op = match ty {
        VarType::Int => Op::Truncate { src, dst },
        VarType::Long => Op::SignExtend { src, dst },
        _ => todo!(),
    };
    ops.push(op);
    d
}

fn convert_cast<'a>(
    (target, exp, ty): (VarType, &typed::Expr<'a>, VarType),
    ops: &mut Vec<Op<'a>>,
    table: &mut SymbolTable<'a>,
) -> Val<'a> {
    match (target, exp, ty) {
        (target, exp, _) if target == exp.typ() => convert_expression(exp, ops, table),
        (_, exp, ty) => {
            let src = convert_expression(exp, ops, table);
            let dst = new_var(ty, r#table);
            // add dst to symbol table
            //
            convert_cast_op(ty, src, dst, ops)
        }
    }
}

fn convert_vardec<'a>(dec: &typed::VarDec<'a>, ops: &mut Vec<Op<'a>>, table: &mut SymbolTable<'a>) {
    if let typed::VarDec {
        name,
        init: Some(init),
        sc: None | Some(StorageClass::Extern),
        typ: _,
    } = dec
    {
        let result = convert_expression(init, ops, table);
        ops.push(Op::Copy {
            src: result,
            dst: Val::Var(*name),
        })
    }
}

fn convert_expression<'a>(
    exp: &typed::Expr<'a>,
    ops: &mut Vec<Op<'a>>,
    table: &mut SymbolTable<'a>,
) -> Val<'a> {
    use typed::Expr;
    match exp {
        Expr::Cast { target, exp, ty } => convert_cast((*target, exp, *ty), ops, table),
        Expr::Const { c, ty: _ } => Val::Constant(*c),
        Expr::FunctionCall { name, args, ty } => {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push(convert_expression(arg, ops, table));
            }
            let result = new_var(*ty, table);
            ops.push(Op::FunCall {
                name: *name,
                args: args_vec.into(),
                dst: result.clone(),
            });
            result
        }
        Expr::Assignment { dst, src, ty: _ } => {
            let &Expr::Var { key: v, .. } = dst.as_ref() else {
                unreachable!()
            };
            let result = convert_expression(src, ops, table);
            let var = Val::Var(v);
            ops.push(Op::Copy {
                src: result,
                dst: var.clone(),
            });
            var
        }
        Expr::Bin(typed::Binary {
            operator,
            left,
            right,
            ty,
        }) => match process_binop(operator) {
            ProcessedBinop::LogAnd => {
                let source_1 = convert_expression(left, ops, table);
                let false_label = Label::and();
                let end_label = Label::end();
                let result = new_var(*ty, table);
                ops.push(Op::JumpIfZero {
                    condition: source_1,
                    target: false_label.clone(),
                });
                let source_2 = convert_expression(right, ops, table);
                ops.extend([
                    Op::JumpIfZero {
                        condition: source_2,
                        target: false_label.clone(),
                    },
                    Op::Copy {
                        src: Val::Constant(Constant::Int(1)),
                        dst: result.clone(),
                    },
                    Op::Jump {
                        target: end_label.clone(),
                    },
                    Op::Label(false_label),
                    Op::Copy {
                        src: Val::Constant(Constant::Int(0)),
                        dst: result.clone(),
                    },
                    Op::Label(end_label.clone()),
                ]);
                result
            }
            ProcessedBinop::LogOr => {
                let source_1 = convert_expression(left, ops, table);
                let true_label = Label::or();
                let end_label = Label::end();
                let result = new_var(*ty, table);
                // if source 1 is true we jump to true label
                ops.push(Op::JumpIfNotZero {
                    condition: source_1,
                    target: true_label.clone(),
                });
                let source_2 = convert_expression(right, ops, table);
                ops.extend([
                    Op::JumpIfNotZero {
                        condition: source_2,
                        target: true_label.clone(),
                    },
                    Op::Copy {
                        src: Val::Constant(Constant::Int(0)),
                        dst: result.clone(),
                    },
                    Op::Jump {
                        target: end_label.clone(),
                    },
                    Op::Label(true_label),
                    Op::Copy {
                        src: Val::Constant(Constant::Int(1)),
                        dst: result.clone(),
                    },
                    Op::Label(end_label),
                ]);
                result
            }

            ProcessedBinop::Normal(operator) => {
                let source_1 = convert_expression(left, ops, table);
                let source_2 = convert_expression(right, ops, table);
                let dst = new_var(*ty, table);
                let binary = Op::Binary {
                    operator,
                    source_1,
                    source_2,
                    dst: dst.clone(),
                };
                ops.push(binary);
                dst
            }
            ProcessedBinop::Compound(op) => {
                let dst = convert_expression(left, ops, table);
                let modifier = convert_expression(right, ops, table);
                let binary = Op::Binary {
                    operator: op.into(),
                    source_1: dst.clone(),
                    source_2: modifier,
                    dst: dst.clone(),
                };
                ops.push(binary);
                dst
            }
        },

        Expr::Conditional {
            condition,
            r#true,
            r#false,
            ty,
        } => {
            let c = convert_expression(condition, ops, table);
            let result = new_var(*ty, table);
            let false_label = Label::conditional();

            let end = Label::conditional();
            ops.push(Op::JumpIfZero {
                condition: c,
                target: false_label.clone(),
            });
            let true_res = convert_expression(r#true, ops, table);
            ops.extend([
                Op::Copy {
                    src: true_res,
                    dst: result.clone(),
                },
                Op::Jump {
                    target: end.clone(),
                },
                Op::Label(false_label),
            ]);
            let false_res = convert_expression(r#false, ops, table);
            ops.extend([
                Op::Copy {
                    src: false_res,
                    dst: result.clone(),
                },
                Op::Label(end),
            ]);
            result
        }
        Expr::Unary(typed::Unary {
            op: operator,
            exp: operand,
            ty,
        }) => {
            let tmp = new_var(*ty, table);
            let factor_result = convert_expression(operand, ops, table);
            let unary = Op::Unary {
                op: *operator,
                source: factor_result,
                dst: tmp.clone(),
            };
            ops.push(unary);
            tmp
        }
        Expr::Nested(e) => convert_expression(e, ops, table),
        Expr::Var { key: v, .. } => Val::Var(*v),
        Expr::IncDec {
            op: IncDec { inc, fix: Fix::Pre },
            ty: _,
            exp,
        } => {
            let expression_result = convert_expression(exp, ops, table);
            let op = match inc {
                IncOp::Inc => Bin::Add,
                IncOp::Dec => Bin::Subtract,
            };

            //prefix
            ops.push(Op::Binary {
                operator: op,
                source_1: expression_result.clone(),
                source_2: Val::Constant(Constant::Int(1)),
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
            let res = convert_expression(exp, ops, table);
            let op = match inc {
                IncOp::Inc => Bin::Add,
                IncOp::Dec => Bin::Subtract,
            };

            let old_val = new_var(*ty, table);
            //prefix
            ops.extend([
                Op::Copy {
                    src: res.clone(),
                    dst: old_val.clone(),
                },
                Op::Binary {
                    operator: op,
                    source_1: res.clone(),
                    source_2: Val::Constant(Constant::Int(1)),
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
    Normal(Bin),
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

impl From<CompoundOp> for Bin {
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

const fn process_binop(binop: &ast::Bop) -> ProcessedBinop {
    use ast::Bop as Pre;
    use ProcessedBinop as Post;
    match binop {
        Pre::Equals => unreachable!(),
        Pre::Ternary => unreachable!(),
        Pre::LogAnd => Post::LogAnd,
        Pre::LogOr => Post::LogOr,
        Pre::Add => Post::Normal(Bin::Add),
        Pre::Subtract => Post::Normal(Bin::Subtract),
        Pre::Multiply => Post::Normal(Bin::Multiply),
        Pre::BitAnd => Post::Normal(Bin::BitAnd),
        Pre::BitOr => Post::Normal(Bin::BitOr),
        Pre::Xor => Post::Normal(Bin::Xor),
        Pre::LeftShift => Post::Normal(Bin::LeftShift),
        Pre::RightShift => Post::Normal(Bin::RightShift),
        Pre::EqualTo => Post::Normal(Bin::EqualTo),
        Pre::NotEqual => Post::Normal(Bin::NotEqual),
        Pre::LessThan => Post::Normal(Bin::LessThan),
        Pre::GreaterThan => Post::Normal(Bin::GreaterThan),
        Pre::Leq => Post::Normal(Bin::Leq),
        Pre::Geq => Post::Normal(Bin::Geq),
        Pre::Divide => Post::Normal(Bin::Divide),
        Pre::Remainder => Post::Normal(Bin::Remainder),
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

fn new_var<'a>(typ: VarType, symbols: &mut SymbolTable) -> Val<'a> {
    let tacky_tmp = tacky_temp();
    symbols.insert(SymKey::Var(tacky_tmp), Attr::Automatic(typ));
    Val::Tmp(tacky_temp())
}

fn tacky_temp() -> TackyTmp {
    TackyTmp::new()
}
