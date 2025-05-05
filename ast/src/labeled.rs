use crate::prelude::*;
use crate::LabelId;
pub mod prelude {
    pub use super::{Block, BlockItem, Dec, FnDec, ForInit, LabelStmnt, Program, Stmnt, VarDec};
    pub use crate::prelude::*;
    pub use crate::LabelId;
}

pub type Program<'a> = Box<[Dec<'a>]>;

#[derive(Debug, Clone)]
pub enum Dec<'a> {
    Var(VarDec<'a>),
    Fn(FnDec<'a>),
}

#[derive(Debug, Clone)]
pub struct FnDec<'a> {
    pub name: Key<'a>,
    pub params: ParamList<'a>,
    pub body: Option<Block<'a>>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

#[derive(Debug, Clone)]
pub struct VarDec<'a> {
    pub name: Key<'a>,
    pub init: Option<Expr<'a>>,
    pub sc: Option<StorageClass>,
    pub typ: VarType,
}

pub type Block<'a> = Box<[BlockItem<'a>]>;

#[derive(Debug, Clone)]
pub enum BlockItem<'a> {
    S(Stmnt<'a>),
    D(Dec<'a>),
}

#[derive(Debug, Clone)]
pub enum Stmnt<'a> {
    Ret(Expr<'a>),
    Exp(Expr<'a>),
    If {
        condition: Expr<'a>,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
    },
    Break(LabelId),
    Continue(LabelId),
    While {
        condition: Expr<'a>,
        body: Box<Self>,
        label: LabelId,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expr<'a>,
        label: LabelId,
    },
    For {
        init: Option<Box<ForInit<'a>>>,
        condition: Option<Expr<'a>>,
        post: Option<Expr<'a>>,
        body: Box<Self>,
        label: LabelId,
    },
    Compound(Block<'a>),
    NamedLabel {
        l: Key<'a>,
        body: Box<Self>,
    },
    Case {
        case: Constant,
        stmnt: LabelStmnt<'a>,
    },
    Default(LabelStmnt<'a>),
    Goto(Key<'a>),
    Switch {
        val: Expr<'a>,
        body: Box<Self>,
        cases: Box<[Constant]>,
        default: bool,
        label: LabelId,
    },
    Null,
}

impl<'a> Stmnt<'a> {
    pub fn with_label(self, id: LabelId) -> LabelStmnt<'a> {
        LabelStmnt {
            id,
            body: Box::new(self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LabelStmnt<'a> {
    pub id: LabelId,
    pub body: Box<Stmnt<'a>>,
}

#[derive(Debug, Clone)]
pub enum ForInit<'a> {
    E(Expr<'a>),
    D(VarDec<'a>),
}
