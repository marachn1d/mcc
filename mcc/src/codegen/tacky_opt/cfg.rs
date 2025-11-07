use super::Instruction;

pub struct Graph<T> {
    pub nodes: Vec<Node<T>>,
}

impl<T> Graph<T> {
    pub fn into_program(mut self) -> Box<[Instruction]> {
        self.nodes.sort_by_key(|node| node.id);
        self.nodes
            .into_iter()
            .map(|node| node.instructions)
            .reduce(|mut instructions, next| {
                instructions.extend(next);
                instructions
            })
            .unwrap()
            .into_boxed_slice()
    }

    pub fn linear_iter_mut<'a>(&'a mut self) -> std::slice::IterMut<'a, Node<T>> {
        self.nodes.iter_mut()
    }

    pub fn find_mut(&mut self, id: &NodeId) -> Option<&mut Node<T>> {
        self.nodes
            .binary_search_by_key(id, |node| node.id)
            .ok()
            .map(|idx| &mut self.nodes[idx])
    }

    #[allow(dead_code)]
    pub fn find(&self, id: &NodeId) -> Option<&Node<T>> {
        self.nodes
            .binary_search_by_key(id, |node| node.id)
            .ok()
            .map(|idx| &self.nodes[idx])
    }

    pub fn dfs<'a>(&'a mut self) -> NodeIter<'a, T> {
        NodeIter {
            next: vec![NodeId::Entry],
            graph: NonNull::from_mut(self),
            _marker: PhantomData,
        }
    }

    pub fn retain(&mut self, f: impl FnMut(&Node<T>) -> bool) {
        self.nodes.retain(f)
    }
}

use std::marker::PhantomData;
use std::ptr::NonNull;
pub struct NodeIter<'a, T> {
    next: Vec<NodeId>,
    graph: NonNull<Graph<T>>,
    _marker: PhantomData<&'a mut Graph<T>>,
}

impl<'a, T> Iterator for NodeIter<'a, T> {
    type Item = &'a mut Node<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(id) = self.next.pop() {
            let node = unsafe { self.graph.as_mut() }.find_mut(&id).unwrap();
            if let Some(successors) = &node.successors {
                self.next.extend(successors);
            }
            Some(node)
        } else {
            None
        }
    }
}

pub struct Node<T> {
    pub id: NodeId,
    pub instructions: Vec<Instruction>,
    pub predecessors: Option<Vec<NodeId>>,
    pub successors: Option<Vec<NodeId>>,
    annot: T,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeId {
    Entry,
    Basic(usize),
    Exit,
}

impl NodeId {
    pub fn basic(val: usize) -> Self {
        Self::Basic(val)
    }
}

impl<T> Node<T> {
    pub fn basic_start(instructions: Vec<Instruction>, id: usize, annot: T) -> Self {
        Self {
            id: NodeId::basic(id),
            instructions,
            annot,
            successors: None,
            predecessors: None,
        }
    }

    pub const fn is_exit(&self) -> bool {
        matches!(self.id, NodeId::Exit)
    }

    pub const fn is_entry(&self) -> bool {
        matches!(self.id, NodeId::Entry)
    }

    pub const fn annot(&self) -> &T {
        &self.annot
    }

    pub const fn annot_mut(&mut self) -> &mut T {
        &mut self.annot
    }

    pub fn entry_node(t: T) -> Self {
        Self {
            id: NodeId::Entry,
            instructions: vec![],
            predecessors: None,
            successors: None,
            annot: t,
        }
    }

    pub fn exit_node(t: T) -> Self {
        Self {
            id: NodeId::Exit,
            instructions: vec![],
            predecessors: None,
            successors: None,
            annot: t,
        }
    }

    pub fn push_predecessor(&mut self, predecessor: NodeId) {
        if let Some(vec) = &mut self.predecessors {
            vec.push(predecessor)
        } else {
            self.predecessors = Some(vec![predecessor])
        }
    }

    pub fn push_successor(&mut self, successor: NodeId) {
        if let Some(vec) = &mut self.successors {
            vec.push(successor)
        } else {
            self.successors = Some(vec![successor])
        }
    }
}

impl<T: Default> Default for Node<T> {
    fn default() -> Self {
        Self::basic_start(vec![], 0, T::default())
    }
}
