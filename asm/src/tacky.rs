use ast::c_vals::*;
use ast::Key;
//use ast::Label;
use ast::UnOp;

#[derive(Debug, Copy, Clone)]
pub enum Label<'a> {
    Anon(ast::Label),
    Named { lbl: Key<'a>, f: Key<'a> },
}

use ast::{LabelId, LabelPos};
impl<'a> Label<'a> {
    pub const fn new(lbl: ast::Label) -> Self {
        Self::Anon(lbl)
    }

    pub const fn named(lbl: Key<'a>, f: Key<'a>) -> Self {
        Self::Named { lbl, f }
    }

    pub fn conditional() -> Self {
        Self::new(LabelId::new().pos(LabelPos::Conditional))
    }

    pub fn start(id: &LabelId) -> Self {
        Self::new(id.start())
    }

    pub fn end() -> Self {
        Self::new(LabelId::new().end())
    }

    pub fn break_(id: &LabelId) -> Self {
        Self::new(id.r#break())
    }

    pub fn continue_(id: &LabelId) -> Self {
        Self::new(id.r#continue())
    }

    pub fn and() -> Self {
        Self::new(LabelId::new().pos(LabelPos::And))
    }

    pub fn or() -> Self {
        Self::new(LabelId::new().pos(LabelPos::Or))
    }

    pub fn if_() -> Self {
        Self::new(LabelId::new().pos(LabelPos::If))
    }

    pub fn else_() -> Self {
        Self::new(LabelId::new().pos(LabelPos::Else))
    }

    pub fn case(id: &LabelId, c: &Constant) -> Self {
        Self::new(id.case(*c))
    }

    pub fn default(id: &LabelId) -> Self {
        Self::new(id.pos(LabelPos::Default))
    }
}

#[derive(Debug)]
pub struct Program<'a>(pub Box<[TopLevel<'a>]>);

#[derive(Debug)]
pub enum TopLevel<'a> {
    Fn(FnDef<'a>),
    StaticVar(StaticVar<'a>),
}

#[derive(Debug)]
pub struct StaticVar<'a> {
    pub name: Key<'a>,
    pub global: bool,
    pub init: StaticInit,
    pub typ: VarType,
}

#[derive(Debug)]
pub struct FnDef<'a> {
    pub name: Key<'a>,
    // empty for x86, which kinda sucks,
    pub params: Box<[Key<'a>]>,
    pub global: bool,
    pub body: Box<[Op<'a>]>,
}

#[derive(Debug)]
pub enum Op<'a> {
    SignExtend {
        src: Val<'a>,
        dst: Val<'a>,
    },

    Truncate {
        src: Val<'a>,
        dst: Val<'a>,
    },
    Return(Val<'a>),
    Unary {
        op: UnOp,
        source: Val<'a>,
        dst: Val<'a>,
    },
    Binary {
        operator: Bin,
        source_1: Val<'a>,
        source_2: Val<'a>,
        dst: Val<'a>,
    },
    Copy {
        src: Val<'a>,
        dst: Val<'a>,
    },
    Jump {
        target: Label<'a>,
    },
    JumpIfZero {
        condition: Val<'a>,
        target: Label<'a>,
    },
    JumpIfNotZero {
        condition: Val<'a>,
        target: Label<'a>,
    },
    Label(Label<'a>),
    FunCall {
        name: Key<'a>,
        args: Box<[Val<'a>]>,
        dst: Val<'a>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum Val<'a> {
    Constant(Constant),
    Var(Key<'a>),
    Tmp(TackyTmp),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Bin {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitOr,
    Xor,
    LeftShift,
    RightShift,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}

use std::sync::atomic::{AtomicUsize, Ordering};
static TACKY_ID: AtomicUsize = AtomicUsize::new(0);
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct TackyTmp(usize);
impl TackyTmp {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for TackyTmp {
    fn default() -> Self {
        Self(TACKY_ID.fetch_add(1, Ordering::AcqRel))
    }
}

impl std::fmt::Display for TackyTmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_tmp_{}", self.0)
    }
}
