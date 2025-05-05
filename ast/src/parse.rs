use super::prelude::*;
use std::fmt::{self, Display, Formatter};
pub mod prelude {
    pub use super::{Block, BlockItem, Dec, FnDec, ForInit, Program, Stmnt, VarDec};
    pub use crate::prelude::*;
}

#[derive(Debug)]
pub struct Program<'a>(pub Box<[Dec<'a>]>);

#[derive(Debug)]
pub enum Dec<'a> {
    Fn(FnDec<'a>),
    Var(VarDec<'a>),
}

#[derive(Debug)]
pub struct FnDec<'a> {
    pub name: Key<'a>,
    pub params: ParamList<'a>,
    pub body: Option<Block<'a>>,
    pub sc: Option<StorageClass>,
    pub typ: FnType,
}

impl<'a> From<FnDec<'a>> for Dec<'a> {
    fn from(dec: FnDec<'a>) -> Self {
        Dec::Fn(dec)
    }
}

impl<'a> From<VarDec<'a>> for Dec<'a> {
    fn from(dec: VarDec<'a>) -> Self {
        Dec::Var(dec)
    }
}
// in my ideal version BlockItems and Params would be like slab allocated
pub type Block<'a> = Box<[BlockItem<'a>]>;

#[derive(Debug)]
pub struct VarDec<'a> {
    pub name: Key<'a>,
    pub init: Option<Expr<'a>>,
    pub sc: Option<StorageClass>,
    pub typ: VarType,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    S(Stmnt<'a>),
    D(Dec<'a>),
}

#[derive(Debug)]
pub enum Stmnt<'a> {
    Ret(Expr<'a>),
    Exp(Expr<'a>),
    If {
        condition: Expr<'a>,
        then: Box<Self>,
        r#else: Option<Box<Self>>,
    },
    Break,
    Continue,
    While {
        condition: Expr<'a>,
        body: Box<Self>,
    },
    DoWhile {
        body: Box<Self>,
        condition: Expr<'a>,
    },
    For {
        init: Option<ForInit<'a>>,
        condition: Option<Expr<'a>>,
        post: Option<Expr<'a>>,
        body: Box<Self>,
    },
    Compound(Block<'a>),
    NamedLabel {
        label: Key<'a>,
        body: Box<Self>,
    },
    Case {
        case: Constant,
        body: Box<Self>,
    },
    Default(Box<Self>),
    Goto(Key<'a>),
    Null,
    Switch {
        val: Expr<'a>,
        body: Box<Self>,
    },
}

#[derive(Debug)]
pub enum ForInit<'a> {
    D(VarDec<'a>),
    E(Expr<'a>),
}

impl Display for Program<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Program(\n{:?}\n)", self.0)
    }
}

impl Display for Stmnt<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Stmnt::Ret(ret) => write!(f, "Return(\n{:?}\n)", ret),
            Stmnt::Exp(e) => write!(f, "(\n{:?}\n)", e),
            Stmnt::Null => write!(f, "(\nnull\n)"),

            Stmnt::If {
                condition,
                then,
                r#else,
            } => write!(f, "if(\n{condition:?}\n){then}{else:?}"),
            Stmnt::NamedLabel { label, body } => write!(f, "LABEL\n{label}:{body}\n"),
            Stmnt::Case { case, body } => write!(f, "Case\n{case}:{body}\n"),
            Stmnt::Default(body) => write!(f, "Default\n{body}\n"),

            Stmnt::Goto(name) => write!(f, "goto\n{name}:\n"),

            Stmnt::Compound(block) => writeln!(f, "{{{block:?}}}"),
            _ => todo!(),
        }
    }
}
