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

fn word_character<T: AsRef<char>>(byte: &T) -> bool {
    // assuming that performance "loss" is worth dedup
    word_start(byte) || byte.as_ref().is_ascii_digit()
}

fn word_start<T: AsRef<char>>(byte: &T) -> bool {
    byte.as_ref().is_ascii_alphanumeric() || *byte.as_ref() == '_'
}

struct DebugToken<Ident> {
    pub token: Token<Ident>,
    pub line: usize,
    pub char: usize,
}

impl<T> From<Constant> for Token<T> {
    fn from(c: Constant) -> Self {
        Self::Const(c)
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
}
