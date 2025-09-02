use super::{Label, LabelId};
use crate::parse;
use crate::Constant;
use crate::{Arr, Ident, IncDec, VarType};
use parse::{Bop, FnType, ParamList, StaticInit, StorageClass, UnOp};
use std::fmt::{self, Display, Formatter};

pub type Program = Arr<Dec>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Dec {
    Var(VarDec),
    Fn(FnDec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDec {
    pub name: Ident,
    pub params: ParamList,
    pub body: Option<Block>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarDec {
    pub name: Ident,
    pub init: Option<Expr>,
    pub sc: Option<StorageClass>,
    pub typ: VarType,
}

pub type Block = Arr<BlockItem>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BlockItem {
    S(Stmnt),
    D(Dec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Goto(Ident),
    Switch {
        val: Expr,
        cases: Arr<SwitchCase>,
        label: LabelId,
    },
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SwitchCase {
    pub case: Option<Case>,
    pub body: Stmnt,
}

impl Display for Case {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Case(c) => {
                write!(f, "{}", c.const_val().unwrap())
            }
            Self::Default => {
                write!(f, "d")
            }
        }
    }
}

impl SwitchCase {
    pub const fn new(case: Option<Case>, body: Stmnt) -> Self {
        Self { case, body }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Case {
    Case(Expr),
    Default,
}

impl Case {
    const fn is_default(&self) -> bool {
        matches!(self, Self::Default)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ForInit {
    E(Expr),
    D(VarDec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        name: Ident,
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
        name: Ident,
        args: Arr<Self>,
        ty: VarType,
    },
}

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

    pub const fn const_val(&self) -> Option<Constant> {
        match self {
            Expr::Const { cnst, .. } => Some(*cnst),
            Expr::Nested { inner: e, .. } => e.const_val(),
            _ => None,
        }
    }
}
