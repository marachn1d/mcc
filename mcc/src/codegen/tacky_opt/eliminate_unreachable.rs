use super::{Graph, Instruction, Node, NodeId};
use std::collections::HashSet;

pub fn eliminate_unreachable<T>(graph: &mut Graph<T>) -> bool {
    let mut visited_nodes = HashSet::new();

    for node in graph.dfs() {
        visited_nodes.insert(node.id);
    }

    let mut eliminated_any = false;

    graph.retain(|node| {
        if !visited_nodes.contains(&node.id) {
            eliminated_any = true;
            false
        } else {
            true
        }
    });

    // now we're gonna get rid of unnecessary labels
    for node in graph.linear_iter_mut().filter(|node| {
        // if we start with a label
        node.instructions
            .first()
            .is_some_and(|op| matches!(op, Instruction::Label(_)))
    }) {
        // and we only have one predecessor
        if let Some([NodeId::Basic(pred_id)]) = node.predecessors.as_ref().map(|p| p.as_slice()) {
            if let NodeId::Basic(our_id) = node.id {
                // and our predecessor directly precedes us
                if *pred_id == our_id - 1 {
                    eliminated_any = true;
                    node.instructions.remove(0);
                }
            }
        }
    }

    eliminated_any
}
