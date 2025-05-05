use crate::prelude::{Constant, FnType, Key, ParamList, StorageClass, VarType};

use crate::LabelId;

pub mod prelude {
    pub use super::*;
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

#[derive(Debug, Clone)]
pub struct LabelStmnt<'a> {
    pub label: LabelId,
    pub body: Box<Stmnt<'a>>,
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
    Bin(Binary<'a>),
    Cast {
        target: VarType,
        ty: VarType,
        exp: Box<Self>,
    },

    //factors
    IncDec {
        op: crate::IncDec,
        exp: Box<Self>,
        ty: VarType,
    },

    Var {
        key: Key<'a>,
        ty: VarType,
    },
    Const {
        c: Constant,

        ty: VarType,
    },
    Unary(Unary<'a>),
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
        ty: VarType,
    },
    FunctionCall {
        name: Key<'a>,
        args: Box<[Self]>,
        ty: VarType,
    },
}

impl Expr<'_> {
    const fn type_mut(&mut self) -> &mut VarType {
        use Expr::{
            Assignment, Bin, Cast, Conditional, Const, FunctionCall, IncDec, Nested, Unary as Un,
            Var,
        };
        match self {
            Assignment { ty, .. }
            | Bin(Binary { ty, .. })
            | Cast { ty, .. }
            | IncDec { ty, .. }
            | Var { ty, .. }
            | Const { ty, .. }
            | Un(Unary { ty, .. })
            | Conditional { ty, .. }
            | FunctionCall { ty, .. } => ty,
            Nested(e) => e.type_mut(),
        }
    }

    pub const fn typ(&self) -> VarType {
        use Expr::{
            Assignment, Bin, Cast, Conditional, Const, FunctionCall, IncDec, Nested, Unary as Un,
            Var,
        };
        match self {
            Assignment { ty, .. }
            | Bin(Binary { ty, .. })
            | Cast { ty, .. }
            | IncDec { ty, .. }
            | Var { ty, .. }
            | Const { ty, .. }
            | Un(Unary { ty, .. })
            | Conditional { ty, .. }
            | FunctionCall { ty, .. } => *ty,
            Nested(e) => e.typ(),
        }
    }

    pub const fn set_typ(&mut self, ty: VarType) {
        *self.type_mut() = ty
    }
}

#[derive(Debug, Clone)]
pub struct Unary<'a> {
    pub exp: Box<Expr<'a>>,
    pub op: crate::UnOp,
    pub ty: VarType,
}

#[derive(Debug, Clone)]
pub struct Binary<'a> {
    pub operator: crate::Bop,
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,

    pub ty: VarType,
}
