use super::labeled_prelude as labeled;
use super::labeled_prelude::*;
use labeled::{Bop, Constant, FnType, Label, ParamList, StaticInit, StorageClass, UnOp, VarType};

use super::{Arr, Identifier, IncDec, Label};
use crate::lex::Constant;
use crate::parse;
use crate::semantics::LabelId;
use parse::{Bop, FnType, ParamList, StorageClass, UnOp, VarType};

pub type Program = Arr<Dec>;

#[derive(Debug, Clone)]
pub enum Dec {
    Var(VarDec),
    Fn(FnDec),
}

#[derive(Debug, Clone)]
pub struct FnDec {
    pub name: Identifier,
    pub params: ParamList,
    pub body: Option<Block>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

#[derive(Debug, Clone)]
pub struct VarDec {
    pub name: Identifier,
    pub init: Option<Expr>,
    pub sc: Option<StorageClass>,
    pub typ: VarType,
}

pub type Block = Arr<BlockItem>;

#[derive(Debug, Clone)]
pub enum BlockItem {
    S(Stmnt),
    D(Dec),
}

#[derive(Debug, Clone)]
pub enum Stmnt {
    Ret(Expr),
    Exp(Expr),
    If {
        condition: Expr,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
    },
    Break(LabelId),
    Continue(LabelId),
    While {
        condition: Expr,
        body: Box<Self>,
        label: LabelId,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expr,
        label: LabelId,
    },
    For {
        init: Option<Box<ForInit>>,
        condition: Option<Expr>,
        post: Option<Expr>,
        body: Box<Self>,
        label: LabelId,
    },
    Compound(Block),
    Label {
        name: Label,
        body: Box<Self>,
    },
    Goto(Identifier),
    Switch {
        val: Expr,
        body: Box<Self>,
        cases: Box<[Constant]>,
        default: bool,
        label: LabelId,
    },
    Null,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    E(Expr),
    D(VarDec),
}

#[derive(Debug, Clone)]
pub enum Expr {
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
        name: Identifier,
        ty: VarType,
    },
    Const {
        cnst: crate::lex::Constant,
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

use super::StaticInit;
impl Expr {
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
