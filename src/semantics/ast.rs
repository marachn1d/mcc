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
pub use parse::{Arr, ParamList};

#[derive(Debug, Copy, Clone)]
pub enum Fix {
    Pre,
    Post,
}

#[derive(Debug, Copy, Clone)]
pub enum IncDec {
    Inc,
    Dec,
}

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

    use super::{Arr, Fix, Identifier, IncDec, Label};
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
            from: Box<Self>,
            to: Box<Self>,
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
            fix: Fix,
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

    fn handle_incdec(expr: parse::Expr) -> Expr {
        use parse::Expr as AE;
        let (fix, op, e) = match expr {
            AE::PostfixIncrement(e) => (Fix::Post, IncDec::Inc, e),
            AE::PrefixIncrement(e) => (Fix::Pre, IncDec::Inc, e),
            AE::PrefixDecrement(e) => (Fix::Pre, IncDec::Dec, e),
            AE::PostfixDecrement(e) => (Fix::Post, IncDec::Dec, e),
            _ => unreachable!(),
        };
        Expr::IncDec {
            fix,
            op,
            exp: e.into(),
        }
    }

    impl From<parse::Expr> for Expr {
        fn from(e: parse::Expr) -> Self {
            use parse::Expr as AE;
            use Expr as E;
            match e {
                AE::Assignment { from: f, to: t } => E::Assignment {
                    from: f.into(),
                    to: t.into(),
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

                expr @ (AE::PostfixIncrement(_)
                | AE::PostfixDecrement(_)
                | AE::PrefixIncrement(_)
                | AE::PrefixDecrement(_)) => handle_incdec(expr),

                AE::Cast { target, exp: e } => E::Cast {
                    target,
                    exp: e.into(),
                },

                AE::Var(v) => E::Var(v),

                AE::Const(c) => E::Const(c),

                AE::Unary(parse::Unary { exp: e, op: o }) => E::Unary {
                    operator: o,
                    operand: e.into(),
                },

                AE::Nested(e) => E::Nested(e.into()),
                AE::FunctionCall { name, args: a } => E::FunctionCall {
                    name,
                    args: a.into_iter().map(E::from).collect(),
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
                Expr::Const(Constant::Integer(i)) => Some(StaticInit::Int(*i)),
                Expr::Nested(e) => e.static_init(),
                _ => None,
            }
        }
    }
}

pub mod typechecked {
    use super::{Arr, Fix, Identifier, IncDec, Label};
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
            from: Box<Self>,
            to: Box<Self>,
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
            fix: Fix,
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
    }
}

#[allow(dead_code)]
pub mod parse_ref {
    use super::{Arr, Fix, IncDec};
    use crate::lex::Identifier;
    use crate::parse::{Bop, FnType, ParamList, StorageClass, UnOp, VarType};
    pub type ProgramRef<'a> = Arr<DecRef<'a>>;

    #[derive(Debug)]
    pub enum DecRef<'a> {
        Var(VarDecRef<'a>),
        Fn(FnDecRef<'a>),
    }

    #[derive(Debug)]
    pub struct FnDecRef<'a> {
        pub name: &'a Identifier,
        pub params: &'a ParamList,
        pub body: Option<BlockRef<'a>>,
        pub sc: &'a Option<StorageClass>,
        pub typ: &'a FnType,
    }

    #[derive(Debug)]
    pub struct VarDecRef<'a> {
        pub name: &'a mut Identifier,
        pub init: Option<ExprRef<'a>>,
        pub sc: &'a mut Option<StorageClass>,
        pub typ: VarType,
    }

    #[derive(Debug)]
    pub enum StmntRef<'a> {
        Ret(ExprRef<'a>),
        Exp(ExprRef<'a>),
        If {
            condition: ExprRef<'a>,
            then: Box<Self>,
            r#else: Option<Box<Self>>,
        },
        Break,
        Continue,
        While {
            condition: ExprRef<'a>,
            body: Box<Self>,
        },
        DoWhile {
            body: Box<Self>,
            condition: ExprRef<'a>,
        },
        For {
            init: Option<Box<ForInitRef<'a>>>,
            condition: Option<ExprRef<'a>>,
            post: Option<ExprRef<'a>>,
            body: Box<Self>,
        },
        Compound(BlockRef<'a>),
        Label {
            name: LabelRef<'a>,
            body: Box<Self>,
        },
        Goto(&'a mut Identifier),
        Switch {
            val: ExprRef<'a>,
            body: Box<Self>,
            cases: &'a mut [crate::lex::Constant],
            default: bool,
        },
        Null,
    }

    pub type BlockRef<'a> = Arr<BlockIRef<'a>>;

    #[derive(Debug)]
    pub enum BlockIRef<'a> {
        S(StmntRef<'a>),
        D(DecRef<'a>),
    }

    #[derive(Debug)]
    pub enum ForInitRef<'a> {
        E(ExprRef<'a>),
        D(VarDecRef<'a>),
    }

    #[derive(Debug)]
    pub enum ExprRef<'a> {
        Assignment {
            from: Box<Self>,
            to: Box<Self>,
        },

        Binary {
            left: Box<Self>,
            operator: &'a mut Bop,
            right: Box<Self>,
        },
        Cast {
            target: &'a mut VarType,
            exp: Box<Self>,
        },

        IncDec {
            fix: &'a mut Fix,
            op: &'a mut IncDec,
            exp: Box<Self>,
        },

        Var(&'a mut Identifier),
        Const(&'a mut crate::lex::Constant),
        Unary {
            operator: &'a mut UnOp,
            operand: Box<Self>,
        },
        Nested(Box<Self>),

        Conditional {
            condition: Box<Self>,
            r#true: Box<Self>,
            r#false: Box<Self>,
        },
        FunctionCall {
            name: &'a mut Identifier,
            args: Arr<Self>,
        },
    }

    use super::labeled;
    fn from_labeled(program: &mut labeled::Program) -> ProgramRef {
        program.iter_mut().map(from_dec).collect()
    }

    fn from_dec(dec: &mut labeled::Dec) -> DecRef {
        match dec {
            labeled::Dec::Var(v) => DecRef::Var(from_vd(v)),
            labeled::Dec::Fn(f) => DecRef::Fn(from_fn(f)),
        }
    }

    fn from_vd(
        labeled::VarDec {
            name,
            init,
            sc,
            typ,
        }: &mut labeled::VarDec,
    ) -> VarDecRef<'_> {
        VarDecRef {
            name,
            init: init.as_mut().map(from_expr),
            sc,
            typ: *typ,
        }
    }

    fn from_fn(
        labeled::FnDec {
            name,
            params,
            body,
            sc,
            typ,
        }: &mut labeled::FnDec,
    ) -> FnDecRef {
        FnDecRef {
            name,
            params,
            body: body.as_mut().map(from_block),
            sc,
            typ,
        }
    }

    #[derive(Debug)]
    pub enum LabelRef<'a> {
        Default,
        Case(&'a mut crate::lex::Constant),
        Named(&'a mut Identifier),
    }

    fn box_expr(e: &mut labeled::Expr) -> Box<ExprRef> {
        Box::from(from_expr(e))
    }

    fn from_expr(e: &mut labeled::Expr) -> ExprRef {
        use labeled::Expr as E;
        use ExprRef as ER;
        match e {
            E::Assignment { from: f, to: t } => ER::Assignment {
                from: from_expr(f).into(),
                to: from_expr(t).into(),
            },

            E::Binary {
                left: l,
                operator: o,
                right: r,
            } => ER::Binary {
                left: box_expr(l),
                operator: o,
                right: box_expr(r),
            },
            E::Cast { target, exp } => ER::Cast {
                target,
                exp: box_expr(exp),
            },

            E::IncDec { fix, op, exp } => ER::IncDec {
                fix,
                op,
                exp: box_expr(exp),
            },

            E::Var(v) => ER::Var(v),
            E::Const(c) => ER::Const(c),
            E::Unary {
                operator,
                operand: o,
            } => ER::Unary {
                operator,
                operand: box_expr(o),
            },
            E::Nested(e) => ER::Nested(box_expr(e)),

            E::Conditional {
                condition: c,
                r#true: t,
                r#false: f,
            } => ER::Conditional {
                condition: box_expr(c),
                r#true: box_expr(t),
                r#false: box_expr(f),
            },
            E::FunctionCall { name, args } => {
                let args = args.iter_mut().map(from_expr).collect();
                ER::FunctionCall { name, args }
            }
        }
    }

    fn from_block(b: &mut Arr<labeled::BlockItem>) -> Arr<BlockIRef> {
        b.iter_mut()
            .map(|b| match b {
                labeled::BlockItem::S(s) => BlockIRef::S(from_stmnt(s)),
                labeled::BlockItem::D(v) => BlockIRef::D(from_dec(v)),
            })
            .collect()
    }

    fn box_stmnt(s: &mut labeled::Stmnt) -> Box<StmntRef> {
        Box::new(from_stmnt(s))
    }

    fn from_stmnt(s: &mut labeled::Stmnt) -> StmntRef {
        use labeled::Stmnt as S;

        use StmntRef as SR;
        match s {
            S::Ret(e) => StmntRef::Ret(from_expr(e)),
            S::Exp(e) => SR::Exp(from_expr(e)),
            S::If {
                condition: c,
                then: t,
                r#else: e,
            } => SR::If {
                condition: from_expr(c),
                then: Box::new(from_stmnt(t)),
                r#else: e.as_mut().map(|e| Box::new(from_stmnt(e.as_mut()))),
            },
            S::Break(_) => SR::Break,
            S::Continue(_) => SR::Continue,
            S::While {
                condition: e,
                body: s,
                label: _,
            } => SR::While {
                condition: from_expr(e),
                body: from_stmnt(s.as_mut()).into(),
            },
            S::DoWhile {
                body: s,
                condition: e,
                label: _,
            } => SR::DoWhile {
                condition: from_expr(e),
                body: from_stmnt(s.as_mut()).into(),
            },
            S::For {
                init: i,
                condition: c,
                post: p,
                body: b,
                label: _,
            } => SR::For {
                body: Box::new(from_stmnt(b)),
                init: from_forinit(i),
                condition: c.as_mut().map(from_expr),
                post: p.as_mut().map(from_expr),
            },
            S::Compound(b) => SR::Compound(from_block(b)),
            S::Label { name: n, body } => SR::Label {
                name: from_label(n),
                body: from_stmnt(body).into(),
            },
            S::Goto(l) => SR::Goto(l),
            S::Switch {
                val,
                body: b,
                cases,
                default: d,
                label: _,
            } => SR::Switch {
                val: from_expr(val),
                body: box_stmnt(b),
                cases,
                default: *d,
            },
            S::Null => SR::Null,
        }
    }

    const fn from_label(n: &mut super::Label) -> LabelRef {
        use super::Label as L;
        match n {
            L::Named(n) => LabelRef::Named(n),
            L::Default(_) => LabelRef::Default,
            L::Case { c, id: _ } => LabelRef::Case(c),
        }
    }

    fn map_opt_box<T, U>(item: &mut Option<Box<T>>, f: impl Fn(&mut T) -> U) -> Option<Box<U>> {
        item.as_mut()
            .map(<Box<T> as AsMut<T>>::as_mut)
            .map(f)
            .map(Box::new)
    }

    fn from_forinit(f: &mut Option<Box<labeled::ForInit>>) -> Option<Box<ForInitRef>> {
        f.as_mut()
            .map(|f| match f.as_mut() {
                labeled::ForInit::E(e) => ForInitRef::E(from_expr(e)),
                labeled::ForInit::D(d) => ForInitRef::D(from_vd(d)),
            })
            .map(Box::new)
    }
}
