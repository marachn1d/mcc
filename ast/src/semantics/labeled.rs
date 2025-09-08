use super::{Label, LabelId};
use crate::parse;
use crate::Constant;
use crate::{Arr, Ident, IncDec, VarType};
use parse::{Bop, FnType, ParamList, StaticInit, StorageClass, UnOp};

pub type Program = Arr<Dec>;

#[derive(Debug, Clone)]
pub enum Dec {
    Var(VarDec),
    Fn(FnDec),
}

#[derive(Debug, Clone)]
pub struct FnDec {
    pub name: Ident,
    pub params: ParamList,
    pub body: Option<Block>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

#[derive(Debug, Clone)]
pub struct VarDec {
    pub name: Ident,
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
    Goto(Ident),
    Switch {
        val: Expr,
        cases: Arr<SwitchCase>,
        label: LabelId,
    },
    Null,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub case: Option<Case>,
    pub body: Stmnt,
}

impl SwitchCase {
    pub const fn new(case: Option<Case>, body: Stmnt) -> Self {
        Self { case, body }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Case {
    Default,
    Case(Expr),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    E(Expr),
    D(VarDec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Assignment {
        dst: Box<Self>,
        src: Box<Self>,
    },

    Binary {
        left: Box<Self>,
        operator: Bop,
        right: Box<Self>,
    },
    Cast {
        target: VarType,
        exp: Box<Self>,
    },

    IncDec {
        op: IncDec,
        exp: Box<Self>,
    },

    Var(Ident),
    Const(Constant),
    Unary {
        operator: UnOp,
        operand: Box<Self>,
    },
    Nested(Box<Self>),

    Conditional {
        condition: Box<Self>,
        r#true: Box<Self>,
        r#false: Box<Self>,
    },
    FunctionCall {
        name: Ident,
        args: Arr<Self>,
    },
}

impl From<parse::Expr> for Expr {
    fn from(e: parse::Expr) -> Self {
        use parse::Expr as AE;
        use Expr as E;
        match e {
            AE::IncDec { op, exp: e } => Self::IncDec { op, exp: e.into() },
            AE::Var(v) => E::Var(v),
            AE::Const(c) => E::Const(c),
            AE::Nested(e) => E::Nested(e.into()),

            AE::Assignment { dst: f, src: t } => E::Assignment {
                dst: f.into(),
                src: t.into(),
            },
            AE::Cast { target, exp: e } => E::Cast {
                target,
                exp: e.into(),
            },
            AE::Unary(parse::Unary { exp: e, op: o }) => E::Unary {
                operator: o,
                operand: e.into(),
            },
            AE::FunctionCall { name, args: a } => E::FunctionCall {
                name,
                args: a.into_iter().map(E::from).collect(),
            },

            AE::Bin(parse::Binary {
                operator,
                left: l,
                right: r,
            }) => E::Binary {
                left: l.into(),
                operator,
                right: r.into(),
            },
            AE::Conditional {
                condition: c,
                r#true: t,
                r#false: f,
            } => E::Conditional {
                condition: c.into(),
                r#true: t.into(),
                r#false: f.into(),
            },
        }
    }
}

impl From<Box<parse::Expr>> for Box<Expr> {
    fn from(e: Box<parse::Expr>) -> Self {
        Box::new(Expr::from(*e))
    }
}

impl Expr {
    pub const fn static_init(&self) -> Option<StaticInit> {
        match self {
            Expr::Const(Constant::Long(l)) => Some(StaticInit::Long(*l)),
            Expr::Const(Constant::Int(i)) => Some(StaticInit::Int(*i)),
            Expr::Nested(e) => e.static_init(),
            _ => None,
        }
    }

    pub const fn as_lvalue(&self) -> Option<&Ident> {
        match self {
            Self::Var(l) => Some(l),
            Self::Nested(inner) | Self::Assignment { dst: inner, .. } => inner.as_lvalue(),
            _ => None,
        }
    }
}
