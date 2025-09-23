#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token<Ident> {
    Int,
    Void,
    Return,
    If,
    Else,
    Goto,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Default,
    Case,
    Static,
    Extern,
    Long,
    Const(Constant),
    Ident(Ident),
    OpenParen,
    CloseParen,
    OpenBrace,
    Semicolon,
    CloseBrace,
    Tilde,
    Decrement,
    Minus,
    Plus,

    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivEqual,
    PercentEqual,
    BitAndEqual,
    BitOrEqual,
    BitXorEqual,

    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Bar,
    Caret,
    Increment,
    LeftShift,
    LeftShiftEqual,
    RightShift,
    RightShiftEqual,
    Not,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqual,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
    Equals,
    Comma,

    QuestionMark,
    Colon,
}

impl<Ident> Token<Ident> {
    pub fn constant(&self) -> bool {
        matches!(self, Self::Const(_))
    }

    pub fn identifier(&self) -> bool {
        matches!(self, Self::Ident(_))
    }
}

use std::fmt::{self, Display, Formatter};

impl<T: fmt::Debug + Display> Display for Token<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Self::Ident(s) = self {
            <T as Display>::fmt(s, f)
        } else {
            <Self as fmt::Debug>::fmt(self, f)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
pub enum Constant {
    Int(i32),
    Long(i64),
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => i.fmt(f),
            Self::Long(l) => l.fmt(f),
        }
    }
}

#[allow(dead_code)]
fn word_character<T: AsRef<char>>(byte: &T) -> bool {
    // assuming that performance "loss" is worth dedup
    word_start(byte) || byte.as_ref().is_ascii_digit()
}

fn word_start<T: AsRef<char>>(byte: &T) -> bool {
    byte.as_ref().is_ascii_alphanumeric() || *byte.as_ref() == '_'
}

pub struct DebugToken<Ident> {
    pub token: Token<Ident>,
    pub line: usize,
    pub char: usize,
}

impl<T> From<Constant> for Token<T> {
    fn from(c: Constant) -> Self {
        Self::Const(c)
    }
}

impl From<i32> for Constant {
    fn from(c: i32) -> Self {
        Self::Int(c)
    }
}

impl From<i64> for Constant {
    fn from(c: i64) -> Self {
        Self::Long(c)
    }
}

impl Constant {
    pub fn int(&self) -> i32 {
        match self {
            Self::Int(i) => *i,
            Self::Long(l) => *l as i32,
        }
    }

    pub fn long(&self) -> i64 {
        match self {
            Self::Int(i) => *i as i64,
            Self::Long(l) => *l,
        }
    }

    pub fn ty(&self) -> crate::VarType {
        match self {
            Self::Int(_) => crate::VarType::Int,
            Self::Long(_) => crate::VarType::Long,
        }
    }

    pub fn with_unop(&self, op: crate::parse::UnOp) -> Self {
        use crate::parse::UnOp;
        match op {
            UnOp::Complement => match self {
                Self::Int(i) => Self::Int(!i),
                Self::Long(l) => Self::Long(!l),
            },

            UnOp::Negate => match self {
                Self::Int(i) => Self::Int(-i),
                Self::Long(l) => Self::Long(-l),
            },

            UnOp::Not => match self {
                Self::Int(0) => Self::Int(1),
                Self::Long(0) => Self::Long(1),
                Self::Int(_) => Self::Int(0),
                Self::Long(_) => Self::Long(0),
            },
        }
    }

    pub fn with_incdec(&self, op: crate::parse::IncDec) -> Self {
        use crate::parse::inc_dec::{POST_DEC, POST_INC, PRE_DEC, PRE_INC};
        match op {
            POST_INC | POST_DEC => *self,
            PRE_INC => match self {
                Self::Int(i) => Self::Int(i.wrapping_add(1)),
                Self::Long(l) => Self::Long(l.wrapping_add(1)),
            },
            PRE_DEC => match self {
                Self::Int(i) => Self::Int(i.wrapping_sub(1)),
                Self::Long(l) => Self::Long(l.wrapping_sub(1)),
            },
        }
    }
}

impl<Ident> DebugToken<Ident> {
    pub fn into_inner(self) -> (Token<Ident>, usize) {
        (self.token, self.line)
    }

    pub const fn line(&self) -> usize {
        self.line
    }
}

impl<Ident> std::ops::Deref for DebugToken<Ident> {
    type Target = Token<Ident>;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}
use std::ops::Deref;

use std::ops::DerefMut;
impl<Ident> AsRef<Token<Ident>> for DebugToken<Ident> {
    fn as_ref(&self) -> &Token<Ident> {
        self.deref()
    }
}

impl<I> AsMut<Token<I>> for DebugToken<I> {
    fn as_mut(&mut self) -> &mut Token<I> {
        self.deref_mut()
    }
}

impl<I> std::ops::DerefMut for DebugToken<I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

impl<I> From<DebugToken<I>> for Token<I> {
    fn from(debug: DebugToken<I>) -> Token<I> {
        debug.token
    }
}

impl From<crate::Ident> for Token<crate::Ident> {
    fn from(i: crate::Ident) -> Self {
        Self::Ident(i)
    }
}
