use asm::tacky::*;
use ast::Ident;
use std::collections::{HashMap, VecDeque};

#[derive(Debug)]
pub struct Graph<T> {
    pub entry: EntryNode<T>,
    pub nodes: Vec<Node<T>>,
    pub exit: ExitNode<T>,
    pub labels: HashMap<Ident, usize>,
}

const fn is_label(op: &Instruction) -> bool {
    matches!(op, Instruction::Label(_))
}

const fn is_node_end(op: &Instruction) -> bool {
    matches!(
        op,
        Instruction::JumpIfZero { .. }
            | Instruction::JumpIfNotZero { .. }
            | Instruction::Jump { .. }
            | Instruction::Return(_)
    )
}

impl Graph<()> {
    pub fn new(ops: &mut VecDeque<Instruction>) -> Self {
        let mut labels: HashMap<Ident, usize> = HashMap::new();
        let mut nodes = Vec::new();
        let mut entry = EntryNode {
            successors: vec![],
            annot: (),
        };
        let mut exit = ExitNode {
            predecessors: vec![],
            annot: (),
        };
        if let Some(instructions) = split_off_group(ops) {
            entry.push_basic_successor(0);

            if let Some(ref lbl) = instructions.start_label {
                labels.insert(lbl.clone(), 0);
            }

            nodes.push(Node {
                id: 0,
                predecessors: vec![],
                successors: vec![],
                instructions,
                annot: (),
            });
        }

        while let Some(instructions) = split_off_group(ops) {
            let id = nodes.len();
            if let Some(ref lbl) = instructions.start_label {
                labels.insert(lbl.clone(), id);
            }
            nodes.push(Node {
                id,
                predecessors: vec![],
                successors: vec![],
                instructions,
                annot: (),
            })
        }

        for node_idx in 0..nodes.len() {
            let following_node = |idx, len| {
                if idx == len - 1 {
                    NodeId::Exit
                } else {
                    NodeId::basic(idx + 1)
                }
            };

            let (successor_a, successor_b) = match &nodes[node_idx].instructions.last_op {
                Some(LastOp::Jump { target }) => (NodeId::basic(labels[target]), None),
                Some(LastOp::JumpIfZero { target, .. } | LastOp::JumpIfNotZero { target, .. }) => {
                    let id = NodeId::basic(labels[target]);
                    let following = following_node(node_idx, nodes.len());
                    if id == following {
                        (id, None)
                    } else {
                        (id, Some(following))
                    }
                }
                Some(LastOp::Return(_)) => (NodeId::Exit, None),
                None => (following_node(node_idx, nodes.len()), None),
            };

            link_nodes(
                &mut nodes,
                &mut entry,
                &mut exit,
                NodeId::basic(node_idx),
                successor_a,
                successor_b,
            );
        }
        Self {
            entry,
            nodes,
            exit,
            labels,
        }
    }
}

fn split_off_group(ops: &mut VecDeque<Instruction>) -> Option<NodeInstructions> {
    let start_label = split_off_start(ops)?;

    let end_idx = ops.len() - 1;
    let idx = ops.iter().position(|x| is_node_end(x)).unwrap_or(end_idx);
    let mut instructions = if idx == end_idx {
        std::mem::take(ops)
    } else {
        // split_off returns the end and keeps the beginning, we wanna keep the end and take the
        // beginning, so we do
        // a replace
        let rest = ops.split_off(idx + 1);
        std::mem::replace(ops, rest)
    };

    let last_op = LastOp::try_from(instructions.pop_back().expect("list empty")).map_or_else(
        |e| {
            instructions.push_back(e);
            None
        },
        |l| Some(l),
    );

    Some(NodeInstructions {
        start_label,
        instructions: instructions.into(),
        last_op,
    })
}

fn split_off_start(ops: &mut VecDeque<Instruction>) -> Option<Option<Ident>> {
    match ops.front() {
        Some(Instruction::Label(_)) => {
            let Some(Instruction::Label(ident)) = ops.pop_front() else {
                unreachable!()
            };
            Some(Some(ident))
        }
        Some(_) => Some(None),
        None => None,
    }
}

fn link_nodes<T>(
    nodes: &mut Vec<Node<T>>,
    entry: &mut EntryNode<T>,
    exit: &mut ExitNode<T>,
    node: NodeId,
    successor_a: NodeId,
    successor_b: Option<NodeId>,
) {
    match node {
        NodeId::Entry => {
            entry.successors.push(successor_a);
            if let Some(b) = successor_b {
                entry.successors.push(b);
            }
        }
        NodeId::Basic(n) => {
            nodes[n].push_successor(successor_a);
            if let Some(b) = successor_b {
                nodes[n].push_successor(b);
            }
        }
        NodeId::Exit => unreachable!(),
    };

    match successor_a {
        NodeId::Entry => unreachable!(),
        NodeId::Basic(a) => {
            nodes[a].push_predecessor(node);
        }
        NodeId::Exit => {
            exit.predecessors.push(node);
        }
    };

    match successor_b {
        Some(NodeId::Entry) => unreachable!(),
        Some(NodeId::Basic(b)) => {
            nodes[b].push_predecessor(node);
        }
        Some(NodeId::Exit) => {
            exit.predecessors.push(node);
        }
        None => (),
    };
}

impl<T> Graph<T> {
    pub fn instructions_iter(mut self) -> impl Iterator<Item = Instruction> {
        self.nodes.sort_by_key(|node| node.id);
        self.nodes
            .into_iter()
            .map(|node| node.instructions.into_iter())
            .flatten()
    }

    pub fn into_ops(self) -> Box<[Instruction]> {
        self.instructions_iter().collect()
    }

    pub fn into_vecdeque(self) -> VecDeque<Instruction> {
        self.instructions_iter().collect()
    }

    pub fn linear_iter_mut<'a>(&'a mut self) -> std::slice::IterMut<'a, Node<T>> {
        self.nodes.iter_mut()
    }

    pub fn find_mut(&mut self, id: NodeId) -> Option<&mut dyn CfgNode<T>> {
        match id {
            NodeId::Entry => Some(&mut self.entry),
            NodeId::Exit => Some(&mut self.exit),
            NodeId::Basic(b) => self
                .nodes
                .binary_search_by_key(&b, |node| node.id)
                .ok()
                .map(|idx| {
                    let node: &mut dyn CfgNode<T> = &mut self.nodes[idx];
                    node
                }),
        }
    }

    #[allow(dead_code)]
    pub fn find(&self, id: usize) -> Option<&Node<T>> {
        self.nodes
            .binary_search_by_key(&id, |node| node.id)
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
pub struct NodeIter<'a, T: 'a> {
    next: Vec<NodeId>,
    graph: NonNull<Graph<T>>,
    _marker: PhantomData<&'a mut Graph<T>>,
}

impl<'a, T: 'a> Iterator for NodeIter<'a, T> {
    type Item = &'a mut dyn CfgNode<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(id) = self.next.pop() {
            let node: &'a mut dyn CfgNode<T> = unsafe { self.graph.as_mut() }.find_mut(id).unwrap();
            self.next
                .extend(node.successors().iter().copied().flatten());
            Some(node)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Node<T> {
    pub id: usize,
    pub predecessors: Vec<NodeId>,
    pub successors: Vec<NodeId>,
    pub instructions: NodeInstructions,
    pub annot: T,
}

#[derive(Debug)]
pub struct NodeInstructions {
    pub start_label: Option<Ident>,
    pub instructions: Vec<Instruction>,
    pub last_op: Option<LastOp>,
}

impl NodeInstructions {
    fn into_iter(self) -> impl Iterator<Item = Instruction> {
        self.start_label
            .map(Instruction::Label)
            .into_iter()
            .chain(self.instructions.into_iter())
            .chain(self.last_op.map(Instruction::from).into_iter())
    }
}

#[derive(Debug)]
pub enum LastOp {
    Jump { target: Ident },
    JumpIfZero { condition: Value, target: Ident },
    JumpIfNotZero { condition: Value, target: Ident },
    Return(Value),
}

impl LastOp{
    pub const fn is_jump(&self) -> bool{
        !matches!(self,Self::Return(_))
    }
}

impl From<LastOp> for Instruction {
    fn from(val: LastOp) -> Self {
        match val {
            LastOp::Jump { target } => Self::Jump { target },
            LastOp::JumpIfZero { target, condition } => Self::JumpIfZero { target, condition },
            LastOp::JumpIfNotZero { target, condition } => {
                Self::JumpIfNotZero { target, condition }
            }
            LastOp::Return(r) => Self::Return(r),
        }
    }
}

impl TryFrom<Instruction> for LastOp {
    type Error = Instruction;

    fn try_from(op: Instruction) -> Result<Self, Instruction> {
        match op {
            Instruction::Jump { target } => Ok(Self::Jump { target }),
            Instruction::JumpIfZero { target, condition } => {
                Ok(Self::JumpIfZero { target, condition })
            }
            Instruction::JumpIfNotZero { target, condition } => {
                Ok(Self::JumpIfNotZero { target, condition })
            }
            Instruction::Return(r) => Ok(Self::Return(r)),
            other => Err(other),
        }
    }
}

#[derive(Debug)]
pub struct EntryNode<T> {
    successors: Vec<NodeId>,
    annot: T,
}

impl<T> EntryNode<T> {
    fn push_basic_successor(&mut self, id: usize) {
        self.successors.push(NodeId::basic(id))
    }
}

#[derive(Debug)]
pub struct ExitNode<T> {
    pub predecessors: Vec<NodeId>,
    pub annot: T,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum NodeId {
    Entry,
    Basic(usize),
    Exit,
}

impl NodeId {
    pub const fn basic(val: usize) -> Self {
        Self::Basic(val)
    }
}

impl<T> Node<T> {
    pub fn new(
        id: usize,
        start_label: Option<Ident>,
        instructions: Vec<Instruction>,
        last_op: Option<LastOp>,
        annot: T,
    ) -> Self {
        Self {
            id,
            predecessors: Vec::new(),
            successors: Vec::new(),
            instructions: NodeInstructions {
                start_label,
                instructions,
                last_op,
            },
            annot,
        }
    }

    pub const fn annot(&self) -> &T {
        &self.annot
    }

    pub const fn annot_mut(&mut self) -> &mut T {
        &mut self.annot
    }

    pub fn push_predecessor(&mut self, predecessor: NodeId) {
        self.predecessors.push(predecessor)
    }

    pub fn push_successor(&mut self, successor: NodeId) {
        self.successors.push(successor)
    }

    pub const fn id(&self) -> NodeId{
        NodeId::basic(self.id)
    }
}

pub trait CfgNode<T> {
    fn id (&self) -> NodeId;
    fn annot(&self) -> &T;
    fn annot_mut(&mut self) -> &mut T;
    fn predecessors(&self) -> Option<&Vec<NodeId>>;
    fn successors(&self) -> Option<&Vec<NodeId>>;
    fn predecessors_mut(&mut self) -> Option<&mut Vec<NodeId>>;
    fn successors_mut(&mut self) -> Option<&mut Vec<NodeId>>;

    fn push_predecessor(&mut self, predecessor: NodeId) {
        if let Some(predecessors) = self.predecessors_mut() {
            predecessors.push(predecessor)
        }
    }

    fn push_successor(&mut self, successor: NodeId) {
        if let Some(successors) = self.successors_mut() {
            successors.push(successor)
        }
    }
}

impl<T> dyn CfgNode<T> {
    pub fn predecessors_iter(&self) -> impl Iterator<Item = NodeId> {
        if let Some(predecessors) = self.predecessors() {
            predecessors.iter()
        } else {
            [].iter()
        }
        .copied()
    }

    pub fn successors_iter(&self) -> impl Iterator<Item = NodeId> {
        if let Some(successors) = self.successors() {
            successors.iter()
        } else {
            [].iter()
        }
        .copied()
    }
}

impl<T> CfgNode<T> for Node<T> {
    fn id(&self) -> NodeId{
        NodeId::basic(self.id)
    }
    fn annot(&self) -> &T {
        self.annot()
    }

    fn annot_mut(&mut self) -> &mut T {
        self.annot_mut()
    }

    fn predecessors(&self) -> Option<&Vec<NodeId>> {
        Some(&self.predecessors)
    }

    fn successors(&self) -> Option<&Vec<NodeId>> {
        Some(&self.successors)
    }

    fn predecessors_mut(&mut self) -> Option<&mut Vec<NodeId>> {
        Some(&mut self.predecessors)
    }

    fn successors_mut(&mut self) -> Option<&mut Vec<NodeId>> {
        Some(&mut self.successors)
    }
}

impl<T> CfgNode<T> for EntryNode<T> {
    fn id(&self) -> NodeId{
        NodeId::Entry
    }
    fn annot(&self) -> &T {
        &self.annot
    }

    fn annot_mut(&mut self) -> &mut T {
        &mut self.annot
    }

    fn predecessors(&self) -> Option<&Vec<NodeId>> {
        None
    }

    fn successors(&self) -> Option<&Vec<NodeId>> {
        Some(&self.successors)
    }

    fn predecessors_mut(&mut self) -> Option<&mut Vec<NodeId>> {
        None
    }

    fn successors_mut(&mut self) -> Option<&mut Vec<NodeId>> {
        Some(&mut self.successors)
    }
}

impl<T> CfgNode<T> for ExitNode<T> {
    fn id(&self) -> NodeId{
        NodeId::Exit
    }
    fn annot(&self) -> &T {
        &self.annot
    }

    fn annot_mut(&mut self) -> &mut T {
        &mut self.annot
    }

    fn successors(&self) -> Option<&Vec<NodeId>> {
        None
    }

    fn predecessors(&self) -> Option<&Vec<NodeId>> {
        Some(&self.predecessors)
    }

    fn successors_mut(&mut self) -> Option<&mut Vec<NodeId>> {
        None
    }

    fn predecessors_mut(&mut self) -> Option<&mut Vec<NodeId>> {
        Some(&mut self.predecessors)
    }
}
