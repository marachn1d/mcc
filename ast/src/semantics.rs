use crate::Constant;
use crate::Ident;
use crate::parse::StaticInit;
use crate::{VarType, parse::FnType};
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LabelId(pub usize);

#[derive(Debug, Clone)]
pub struct StatementLabels {
    pub start: Ident,
    pub r#break: Ident,
    pub r#continue: Ident,
    pub end: Ident,
}
impl LabelId {
    pub fn labels(&self) -> StatementLabels {
        StatementLabels {
            start: Ident::from(format!("s{}s", self.0)),
            r#break: self.r#break(),
            r#continue: self.r#continue(),
            end: Ident::from(format!("s{}e", self.0)),
        }
    }

    pub fn r#break(&self) -> Ident {
        Ident::from(format!("s{}b", self.0))
    }

    pub fn case(&self, value: Constant) -> Ident {
        Ident::from(format!("sw{}c{}", self.0, value,))
    }
    pub fn default(&self) -> Ident {
        Ident::from(format!("sw{}cd", self.0))
    }
    pub fn r#continue(&self) -> Ident {
        Ident::from(format!("s{}c", self.0))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Label {
    Named(Ident),
    Default(LabelId),
    Case { c: Constant, id: LabelId },
}

pub type SymbolTable = HashMap<Ident, Attr>;

#[derive(Debug)]
pub enum Attr {
    Static {
        typ: VarType,
        init: Option<InitialVal>,
        global: bool,
    },
    Automatic(VarType),
    Fn {
        defined: bool,
        global: bool,
        typ: FnType,
    },
}

#[derive(Debug)]
pub enum Expected {
    VarType,
    FnType,
}

impl Attr {
    pub const fn global(&self) -> bool {
        match self {
            Self::Static { global, .. } | Self::Fn { global, .. } => *global,
            Self::Automatic(_) => false,
        }
    }

    pub const fn var_type(&self) -> Result<&VarType, Expected> {
        match self {
            Self::Static { typ, .. } | Self::Automatic(typ) => Ok(typ),
            Self::Fn { .. } => Err(Expected::VarType),
        }
    }

    pub const fn fn_type(&self) -> Result<&FnType, Expected> {
        match self {
            Self::Fn { typ, .. } => Ok(typ),
            Self::Static { .. } | Self::Automatic(_) => Err(Expected::FnType),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum InitialVal {
    Tentative,
    Initial(StaticInit),
}

impl InitialVal {
    pub const fn get_static(&self, ty: VarType) -> StaticInit {
        match self {
            InitialVal::Initial(i) => *i,
            InitialVal::Tentative => StaticInit::zeroed(ty),
        }
    }

    pub fn as_long(&self) -> i64 {
        match self {
            Self::Initial(si) => si.as_long(),
            Self::Tentative => 0,
        }
    }

    pub fn as_int(&self) -> i32 {
        match self {
            Self::Initial(si) => si.as_int(),
            Self::Tentative => 0,
        }
    }
}

pub mod labeled {
    use super::{Label, LabelId};
    use crate::Constant;
    use crate::parse;
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
            use Expr as E;
            use parse::Expr as AE;
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
        pub fn static_init(&self) -> Option<StaticInit> {
            match self {
                Expr::Const(c) => Some(c.static_init()),
                Expr::Nested(e) => e.static_init(),
                _ => None,
            }
        }
    }
}

pub mod typed {
    use super::{Label, LabelId};
    use crate::Constant;
    use crate::parse;
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

        pub fn static_init(&self) -> Option<StaticInit> {
            match self {
                Expr::Const { cnst, .. } => Some(cnst.static_init()),
                Expr::Nested { inner, .. } => inner.static_init(),
                _ => None,
            }
        }
    }
}
