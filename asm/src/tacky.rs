use ast::parse::{StaticInit, UnOp};
use ast::VarType;
use ast::{Constant, Ident};

//pub type Program = super::Program<Instruction>;
//pub type FunctionDefinition = super::FunctionDefinition<Instruction>;

#[derive(Debug)]
pub struct Program(pub Box<[TopLevel]>);

#[derive(Debug)]
pub enum TopLevel {
    Fn(FunctionDefinition),
    StaticVar(StaticVar),
}

#[derive(Debug)]
pub struct StaticVar {
    pub name: Ident,
    pub global: bool,
    pub init: StaticInit,
    pub typ: VarType,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Ident,
    // empty for x86, which kinda sucks,
    pub params: Box<[Ident]>,
    pub global: bool,
    pub body: Box<[Instruction]>,
}

#[derive(Debug)]
pub enum Instruction {
    SignExtend {
        src: Value,
        dst: Value,
    },

    Truncate {
        src: Value,
        dst: Value,
    },
    Return(Value),
    Unary {
        op: UnOp,
        source: Value,
        dst: Ident,
    },
    Binary {
        operator: TackyBinary,
        source_1: Value,
        source_2: Value,
        dst: Value,
    },
    Copy {
        src: Value,
        dst: Value,
    },
    Jump {
        target: Ident,
    },
    JumpIfZero {
        condition: Value,
        target: Ident,
    },
    JumpIfNotZero {
        condition: Value,
        target: Ident,
    },
    Label(Ident),
    FunCall {
        name: Ident,
        args: Box<[Value]>,
        dst: Value,
    },
}

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Var(Ident),
}

impl Value {
    pub const fn constant(&self) -> Option<&Constant> {
        if let Self::Constant(s) = self {
            Some(s)
        } else {
            None
        }
    }
    pub const fn var(&self) -> Option<&Ident> {
        if let Self::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TackyBinary {
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

use std::num::Wrapping;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

impl TackyBinary {
    // most evil function i've ever written
    pub fn apply<T>(&self, lhs: T, rhs: T) -> T
    where
        Wrapping<T>: Add<Output = Wrapping<T>>
            + Sub<Output = Wrapping<T>>
            + Mul<Output = Wrapping<T>>
            + Div<Output = Wrapping<T>>
            + Rem<Output = Wrapping<T>>
            + BitAnd<Output = Wrapping<T>>
            + BitOr<Output = Wrapping<T>>
            + BitXor<Output = Wrapping<T>>
            + Shl<usize, Output = Wrapping<T>>
            + Shr<usize, Output = Wrapping<T>>,
        T: From<u8> + Ord + TryInto<usize>,
    {
        let (lhs, rhs) = (Wrapping(lhs), Wrapping(rhs));
        let truee = || <u8 as Into<T>>::into(1);
        let falsee = || <u8 as Into<T>>::into(0);
        match self {
            TackyBinary::Add => (lhs + rhs).0,
            TackyBinary::Subtract => (lhs - rhs).0,
            TackyBinary::Multiply => (lhs * rhs).0,
            TackyBinary::Divide => (lhs / rhs).0,
            TackyBinary::Remainder => (lhs % rhs).0,
            TackyBinary::BitAnd => (lhs & rhs).0,
            TackyBinary::BitOr => (lhs | rhs).0,
            TackyBinary::Xor => (lhs ^ rhs).0,
            // UNDEFINED BEHAVIOUR HERE: IF THEY TRY TO BITSHIFT BY A NEGATIVE NUMBER WE GIVE THEM
            // ZERO
            TackyBinary::LeftShift => (lhs << rhs.0.try_into().unwrap_or(0)).0,
            TackyBinary::RightShift => (lhs >> rhs.0.try_into().unwrap_or(0)).0,
            TackyBinary::LessThan => {
                if lhs < rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::GreaterThan => {
                if lhs > rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::Leq => {
                if lhs <= rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::Geq => {
                if lhs >= rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::EqualTo => {
                if lhs == rhs {
                    truee()
                } else {
                    falsee()
                }
            }
            TackyBinary::NotEqual => {
                if lhs != rhs {
                    truee()
                } else {
                    falsee()
                }
            }
        }
    }
}
