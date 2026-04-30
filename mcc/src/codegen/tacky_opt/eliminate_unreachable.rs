use super::cfg::LastOp;
use super::{Graph, Instruction, Node, NodeId};
use std::collections::HashSet;

pub fn eliminate_unreachable<T>(graph: &mut Graph<T>) -> bool {
    let mut visited_nodes = HashSet::new();

    for node in graph.dfs() {
        visited_nodes.insert(node.id());
    }

    let mut eliminated_any = false;

    graph.nodes.retain(|node| {
        if !node_is_reachable(node, &visited_nodes) {
            eliminated_any = true;
            if let Some(idx) = graph
                .exit
                .predecessors
                .iter()
                .position(|id| *id == node.id())
            {
                graph.exit.predecessors.swap_remove(idx);
            }
            false
        } else {
            true
        }
    });

    // unnecessary jumps now
    for i in 0..graph.nodes.len() - 1 {
        let successor_id = graph.nodes[i + 1].id();
        let node = &mut graph.nodes[i];
        if node
            .instructions
            .last_op
            .as_ref()
            .is_some_and(LastOp::is_jump)
        {
            match node.successors.as_slice() {
                [b] if *b == successor_id => {
                    node.instructions.last_op = None;
                }
                _ => (),
            }
        }
    }

    // now we're gonna get rid of unnecessary labels
    for node in graph.linear_iter_mut().filter(|node| {
        // if we start with a label
        node.instructions.start_label.is_some()
    }) {
        match node.predecessors.as_slice(){
            [NodeId::Basic(b)] if *b == node.id.saturating_sub(1) => {
                eliminated_any = true;
                node.instructions.start_label = None;
            }
            _ => (),
        }
    }
    eliminated_any
}

fn node_is_reachable<T>(node: &Node<T>, visited_nodes: &HashSet<NodeId>) -> bool {
    visited_nodes.contains(&node.id())
}
