use ast::c_vals::*;
use ast::Key;
use ast::Label;
use ast::UnOp;

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
        dst: Key<'a>,
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
        target: Label,
    },
    JumpIfZero {
        condition: Val<'a>,
        target: Label,
    },
    JumpIfNotZero {
        condition: Val<'a>,
        target: Label,
    },
    Label(Label),
    FunCall {
        name: Key<'a>,
        args: Box<[Val<'a>]>,
        dst: Val<'a>,
    },
}

#[derive(Clone, Debug)]
pub enum Val<'a> {
    Constant(Constant),
    Var(Key<'a>),
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
