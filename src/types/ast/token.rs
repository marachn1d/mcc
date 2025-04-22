use super::super::symbol_table::Key;
use ascii::AsciiStr;
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Token<'a> {
    // Keywords
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
    Ident(Key<'a>),
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

impl Token<'_> {
    pub const fn identifier(&self) -> bool {
        matches!(self, Self::Ident(_))
    }
    pub const fn constant(&self) -> bool {
        matches!(self, Self::Const(_))
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
pub enum Constant {
    Int(i32),
    UInt(u32),
    Long(i64),
    ULong(u64),
}

#[derive(Debug, Clone)]
pub struct DebugToken<'a> {
    pub token: Token<'a>,
    pub line: usize,
}

impl<'a> DebugToken<'a> {
    pub fn into_inner(self) -> (Token<'a>, usize) {
        (self.token, self.line)
    }

    pub const fn line(&self) -> usize {
        self.line
    }
}

impl<'a> std::ops::Deref for DebugToken<'a> {
    type Target = Token<'a>;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}
use std::ops::Deref;

use std::ops::DerefMut;
impl<'a> AsRef<Token<'a>> for DebugToken<'a> {
    fn as_ref(&self) -> &Token<'a> {
        self.deref()
    }
}

impl<'a> AsMut<Token<'a>> for DebugToken<'a> {
    fn as_mut(&mut self) -> &mut Token<'a> {
        self.deref_mut()
    }
}

impl<'a> std::ops::DerefMut for DebugToken<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

impl<'a> From<DebugToken<'a>> for Token<'a> {
    fn from(debug: DebugToken) -> Token {
        debug.token
    }
}
