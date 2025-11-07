use crate::Optimizations;
use asm::tacky::*;
use ast::parse::StaticInit;
use ast::semantics::Label;
use ast::semantics::SymbolTable;
use ast::Ident;
use ast::VarType;
use std::collections::{HashMap, HashSet};
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
    let mut body = body.into_vec();
    while changed {
        changed = false;
        if opt.constant_folding {
            changed &= constant_fold::constant_fold(&mut body);
        }

        if opt.unreachable_code || opt.copy_propogation || opt.dead_store {
            let mut cfg = control_flow_graph(mem::take(&mut body));
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
                body = cfg.into_program().into_vec()
            }
        }
    }
    body.into_boxed_slice()
}

fn propagate_copies<T>(_tacky: &mut Graph<T>) -> bool {
    false
}

fn eliminate_dead_stores<T>(_tacky: &mut Graph<T>) -> bool {
    false
}

// I should turn this into a method on Graph, and give it the map
fn control_flow_graph(body: impl IntoIterator<Item = Instruction>) -> Graph<()> {
    let (nodes, ids) = build_nodes(body);
    add_edges(nodes, ids)
}

fn build_nodes(
    body: impl IntoIterator<Item = Instruction>,
) -> (Vec<Node<()>>, HashMap<Ident, usize>) {
    let mut finished_blocks: Vec<Node<()>> = vec![];
    let mut current_block: Node<()> = Node::basic_start(vec![], 0, ());
    let mut map = HashMap::new();
    // should probably do this differently, like a map or something
    for instruction in body {
        use Instruction::{Jump, JumpIfNotZero, JumpIfZero, Label, Return};
        match instruction {
            Label(ref lbl) => {
                if !current_block.instructions.is_empty() {
                    finished_blocks.push(mem::take(&mut current_block));
                }

                let size = finished_blocks.len();
                map.insert(lbl.clone(), size);
                current_block.id = NodeId::basic(finished_blocks.len());

                current_block.instructions.push(instruction);
            }
            Jump { .. } | JumpIfZero { .. } | JumpIfNotZero { .. } | Return(_) => {
                current_block.instructions.push(instruction);
                finished_blocks.push(mem::take(&mut current_block));
            }
            _ => current_block.instructions.push(instruction),
        }
    }
    if !current_block.instructions.is_empty() {
        finished_blocks.push(current_block)
    }
    (finished_blocks, map)
}

fn link_nodes<T>(predecessor: &mut Node<T>, successor: &mut Node<T>) {
    predecessor.push_successor(successor.id);
    successor.push_predecessor(predecessor.id);
}

fn add_edges(mut basic_blocks: Vec<Node<()>>, ids: HashMap<String, usize>) -> Graph<()> {
    let max_id = basic_blocks.len() - 1;

    let mut entry_node = Node::entry_node(());
    link_nodes(&mut entry_node, &mut basic_blocks[0]);
    //basic_blocks.push(entry_node);

    let mut exit_node = Node::exit_node(());

    /*
     * okay so we're gonna do this without refcell or Rc, so we're gonna be splitting borrows
     * the process is gonna be as follows:
     *  - each block will have either 1 or 2 exits
     *  - for each pair from 0 to max id
     *  - determine the indices of the exit nodes
     */

    // it'd be nice to dedup this, but it'd confuse borrowck since we'd pass &mut exit_node every
    // time, but only modify it sometimes
    for cur_idx in 0..=max_id {
        let (next_id, branch_id) = possible_exits(cur_idx, max_id, &basic_blocks, &ids);
        match (next_id, branch_id) {
            (NodeId::Exit, Some(NodeId::Exit)) => {
                link_nodes(&mut basic_blocks[cur_idx], &mut exit_node);
            }
            (NodeId::Basic(next_idx), Some(NodeId::Basic(branch_idx))) => {
                if next_idx == branch_idx {
                    let [cur, next] = basic_blocks.get_disjoint_mut([cur_idx, next_idx]).unwrap();
                    link_nodes(cur, next);
                } else {
                    let [cur, next, branch] = basic_blocks
                        .get_disjoint_mut([cur_idx, next_idx, branch_idx])
                        .unwrap();
                    link_nodes(cur, next);
                    link_nodes(cur, branch);
                }
            }
            (NodeId::Exit, Some(NodeId::Basic(next_idx)))
            | (NodeId::Basic(next_idx), Some(NodeId::Exit)) => {
                let [cur, next] = basic_blocks.get_disjoint_mut([cur_idx, next_idx]).unwrap();
                link_nodes(cur, next);
                link_nodes(cur, &mut exit_node);
            }
            (NodeId::Basic(next_idx), None) => {
                let [cur, next] = basic_blocks.get_disjoint_mut([cur_idx, next_idx]).unwrap();
                link_nodes(cur, next);
                link_nodes(cur, &mut exit_node)
            }

            (NodeId::Exit, None) => link_nodes(&mut basic_blocks[cur_idx], &mut exit_node),
            (NodeId::Entry, _) => unreachable!(),
            (_, Some(NodeId::Entry)) => unreachable!(),
        }
    }

    basic_blocks.insert(0, entry_node);

    basic_blocks.push(exit_node);
    Graph {
        nodes: basic_blocks,
    }
}

fn possible_exits(
    op_idx: usize,
    max_id: usize,
    blocks: &[Node<()>],
    label_ids: &HashMap<Ident, usize>,
) -> (NodeId, Option<NodeId>) {
    let following_block = if op_idx == max_id {
        NodeId::Exit
    } else {
        NodeId::basic(op_idx + 1)
    };
    use Instruction::{Jump, JumpIfNotZero, JumpIfZero, Return};
    match blocks[op_idx].instructions.last().unwrap() {
        Return(_) => (NodeId::Exit, None),
        Jump { target } => (NodeId::basic(label_ids[target]), None),
        JumpIfZero { target, .. } | JumpIfNotZero { target, .. } => {
            let target_block = NodeId::basic(label_ids[target]);
            if target_block == following_block {
                (following_block, None)
            } else {
                (following_block, Some(target_block))
            }
        }
        _ => (following_block, None),
    }
}
