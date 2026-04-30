
use crate::Optimizations;
use asm::tacky::*;
use ast::parse::StaticInit;
use ast::semantics::Label;
use ast::semantics::SymbolTable;
use ast::Ident;
use ast::VarType;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem;
mod cfg;
use cfg::{Graph, Node, NodeId};
mod constant_fold;
mod eliminate_unreachable;

pub fn opt(tacky: Program, opt: &Optimizations, table: &SymbolTable) -> Program {
    if opt.all_disabled() {
        return tacky;
    }
    map_bodies(tacky, opt, table, optimize)
}

fn map_bodies(
    tacky: Program,
    opt: &Optimizations,
    table: &SymbolTable,
    mut f: impl FnMut(Box<[Instruction]>, &Optimizations, &SymbolTable) -> Box<[Instruction]>,
) -> Program {
    Program(
        tacky
            .0
            .into_iter()
            .map(|tl| {
                if let TopLevel::Fn(FunctionDefinition {
                    body,
                    name,
                    params,
                    global,
                }) = tl
                {
                    TopLevel::Fn(FunctionDefinition {
                        body: f(body, opt, table),
                        name,
                        params,
                        global,
                    })
                } else {
                    tl
                }
            })
            .collect(),
    )
}

fn optimize(
    body: Box<[Instruction]>,
    opt: &Optimizations,
    _table: &SymbolTable,
) -> Box<[Instruction]> {
    let mut changed = true;
    let mut body =  VecDeque::from_iter(body.into_iter());
    while changed {
        changed = false;
        if opt.constant_folding {
            changed &= constant_fold::constant_fold(&mut body);
        }

        if opt.unreachable_code || opt.copy_propogation || opt.dead_store {
            let mut cfg = Graph::new(&mut body);
            if opt.unreachable_code {
                changed |= eliminate_unreachable::eliminate_unreachable(&mut cfg);
            }
            if opt.copy_propogation {
                changed |= propagate_copies(&mut cfg);
            }
            if opt.dead_store {
                changed |= eliminate_dead_stores(&mut cfg);
            }
            if changed {
                body = cfg.into_vecdeque();
            }
        }
    }
    body.into_iter().collect()
}

fn propagate_copies<T>(_tacky: &mut Graph<T>) -> bool {
    false
}

fn eliminate_dead_stores<T>(_tacky: &mut Graph<T>) -> bool {
    false
}

