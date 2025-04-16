/*
 *  TODO:
 *
 *  map_enum crate? like metaprogramming to create a new type based on the enum
 *
 *  omg okay so i think my ideal version would be like a union of ControlFlow and logic, like I
 *  wish I could concisely decide to pass all the arguments or do things with the arguments right
 *  there
 *
 */

use super::StaticInit;
pub use crate::lex::Identifier;
use crate::parse;
pub use parse::inc_dec::*;
pub use parse::{Arr, ParamList};

use super::LabelId;
use crate::lex::Constant;
#[derive(Clone, Debug)]
pub enum Label {
    Named(Identifier),
    Default(LabelId),
    Case { c: Constant, id: LabelId },
}

pub mod label_prelude {
    pub use super::labeled::{Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec};
    pub use super::{Arr, Fix, Identifier, IncDec, Label, ParamList};
}

pub mod type_prelude {
    pub use super::typechecked::{
        Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec,
    };
    pub use super::{Arr, Fix, Identifier, IncDec, Label, ParamList};
}

pub mod labeled {

    use super::{Arr, Identifier, IncDec, Label};
    use crate::lex::Constant;
    use crate::parse;
    use parse::{Bop, FnType, ParamList, StorageClass, UnOp, VarType};

    use crate::semantics::LabelId;

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

        Var(Identifier),
        Const(crate::lex::Constant),
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
            name: Identifier,
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

    use super::StaticInit;
    impl Expr {
        pub const fn static_init(&self) -> Option<StaticInit> {
            match self {
                Expr::Const(Constant::Long(l)) => Some(StaticInit::Long(*l)),
                Expr::Const(Constant::Int(i)) => Some(StaticInit::Int(*i)),
                Expr::Nested(e) => e.static_init(),
                _ => None,
            }
        }
    }
}

pub mod typechecked {
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
}
