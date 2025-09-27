use std::ops::Add;

use crate::Optimizations;
use asm::tacky::*;
use ast::parse::StaticInit;
use ast::semantics::SymbolTable;
use ast::VarType;

pub fn opt(tacky: &mut Program, opt: &Optimizations, table: &SymbolTable) {
    if opt.all_disabled() {
        return;
    }

    let mut function_bodies: Vec<_> = tacky
        .0
        .iter_mut()
        .filter_map(|tl| match tl {
            TopLevel::Fn(FunctionDefinition { body, .. }) => Some(body),
            _ => None,
        })
        .collect();

    for body in function_bodies {
        optimize(body, opt, table)
    }
}

fn optimize(body: &mut Box<[Instruction]>, opt: &Optimizations, table: &SymbolTable) {
    let mut changed = true;
    while changed {
        changed = false;
        if opt.constant_folding {
            changed &= constant_fold(body);
        }

        if opt.unreachable_code || opt.copy_propogation || opt.dead_store {
            let mut cfg = control_flow_graph(body);
            if opt.unreachable_code {
                changed |= eliminate_unreachable(&mut cfg);
            }
            if opt.copy_propogation {
                changed |= propagate_copies(&mut cfg);
            }
            if opt.dead_store {
                changed |= propagate_copies(&mut cfg);
            }
            if changed {
                *body = cfg.into_program()
            }
        }
    }
}

fn constant_fold(tacky: &mut [Instruction]) -> bool {
    let mut changed = false;
    for instruction in tacky {
        use Value::Constant;
        changed |= match instruction {
            Instruction::SignExtend {
                src: Constant(s),
                dst: d,
            } => {
                let src: StaticInit = (*s).into();
                let src: ast::Constant = src.as_long().into();
                *instruction = Instruction::Copy {
                    src: Constant(src),
                    dst: d.clone(),
                };
                true
            }
            Instruction::Truncate {
                src: Constant(s),
                dst: d,
            } => {
                let src: StaticInit = (*s).into();
                let src: ast::Constant = src.as_int().into();
                *instruction = Instruction::Copy {
                    src: Constant(src),
                    dst: d.clone(),
                };
                true
            }

            Instruction::Unary {
                op,
                source: Constant(s),
                dst,
            } => {
                *instruction = Instruction::Copy {
                    dst: Value::Var(dst.clone()),
                    src: Value::Constant(s.with_unop(*op)),
                };
                true
            }
            Instruction::Binary {
                operator,
                source_1: Constant(s1),
                source_2: Constant(s2),
                dst,
            } => {
                let [s1, s2]: [StaticInit; 2] = [*s1, *s2].map(|x| x.into());
                let ty = s1.common_type(&s2);
                let src = match ty {
                    VarType::Int => Value::Constant(ast::Constant::Int(
                        operator.apply(s1.as_int(), s2.as_int()),
                    )),
                    VarType::Long => Value::Constant(ast::Constant::Long(
                        operator.apply(s1.as_long(), s2.as_long()),
                    )),
                };
                *instruction = Instruction::Copy {
                    src,
                    dst: dst.clone(),
                };
                true
            }
            Instruction::JumpIfZero {
                condition: Constant(c),
                target,
            } if c.is_zero() => {
                *instruction = Instruction::Jump {
                    target: target.clone(),
                };
                true
            }
            // need to replace the boxed slice with a vec for this
            Instruction::JumpIfNotZero {
                condition: Constant(_c),
                target: _t,
            } => false,

            _ => false,
        }
    }
    changed
}

fn eliminate_unreachable(tacky: &mut TackyFlow) -> bool {
    false
}

fn propagate_copies(tacky: &mut TackyFlow) -> bool {
    false
}

fn eliminate_dead_stores(tacky: &mut TackyFlow) -> bool {
    false
}

fn control_flow_graph(body: &[Instruction]) -> TackyFlow {
    todo!()
}

struct TackyFlow {}

impl TackyFlow {
    fn into_program(&self) -> Box<[Instruction]> {
        todo!()
    }
}
