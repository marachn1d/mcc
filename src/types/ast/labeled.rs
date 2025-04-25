pub use super::incdec::*;

use super::parse_prelude as parse;
use super::{Constant, FnType, Key, Label, LabelId, StaticInit, VarType};
use parse::{Bop, ParamList, StorageClass, UnOp};
use IncDec;

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

    Var(Key<'a>),
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
        name: Key<'a>,
        args: Box<[Self]>,
    },
}

impl<'a> From<parse::Expr<'a>> for Expr<'a> {
    fn from(e: parse::Expr<'a>) -> Self {
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

impl<'a> From<Box<parse::Expr<'a>>> for Box<Expr<'a>> {
    fn from(e: Box<parse::Expr<'a>>) -> Self {
        Box::new(Expr::from(*e))
    }
}

impl Expr<'_> {
    pub const fn static_init(&self) -> Option<StaticInit> {
        match self {
            Expr::Const(Constant::Long(l)) => Some(StaticInit::Long(*l)),
            Expr::Const(Constant::Int(i)) => Some(StaticInit::Int(*i)),
            Expr::Nested(e) => e.static_init(),
            _ => None,
        }
    }
}
