use super::labeled_prelude as labeled;
use super::labeled_prelude::*;
use super::{Constant, FnType, Key, Label, LabelId, VarType};
use labeled::{Bop, ParamList, StorageClass, UnOp};
pub mod prelude {
    pub use super::super::{Label, LabelId};
    pub use super::{Block, BlockItem, Dec, Expr, FnDec, ForInit, Stmnt, VarDec};
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
    Label {
        name: Label,
        body: Box<Self>,
    },
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

#[derive(Debug, Clone)]
pub enum ForInit<'a> {
    E(Expr<'a>),
    D(VarDec<'a>),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Assignment {
        dst: Box<Self>,
        src: Box<Self>,
        ty: VarType,
    },

    Binary {
        left: Box<Self>,
        operator: Bop,
        right: Box<Self>,

        ty: VarType,
    },
    Cast {
        target: VarType,
        exp: Box<Self>,

        ty: VarType,
    },

    IncDec {
        op: IncDec,
        exp: Box<Self>,

        ty: VarType,
    },

    Var {
        name: Key<'a>,
        ty: VarType,
    },
    Const {
        cnst: Constant,
        ty: VarType,
    },
    Unary {
        operator: UnOp,
        operand: Box<Self>,
        ty: VarType,
    },
    Nested {
        inner: Box<Self>,
        ty: VarType,
    },

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
        ty: VarType,
    },
    FunctionCall {
        name: Identifier,
        args: Arr<Self>,
        ty: VarType,
    },
}

impl Expr<'_> {
    pub const fn ty(&self) -> VarType {
        match self {
            Self::Assignment { ty, .. }
            | Self::Binary { ty, .. }
            | Self::Cast { ty, .. }
            | Self::IncDec { ty, .. }
            | Self::Var { ty, .. }
            | Self::Const { ty, .. }
            | Self::Unary { ty, .. }
            | Self::Nested { ty, .. }
            | Self::Conditional { ty, .. }
            | Self::FunctionCall { ty, .. } => *ty,
        }
    }

    pub const fn static_init(&self) -> Option<StaticInit> {
        match self {
            Expr::Const {
                cnst: Constant::Long(l),
                ..
            } => Some(StaticInit::Long(*l)),
            Expr::Const {
                cnst: Constant::Int(i),
                ..
            } => Some(StaticInit::Int(*i)),
            Expr::Nested { inner: e, .. } => e.static_init(),
            _ => None,
        }
    }
}
