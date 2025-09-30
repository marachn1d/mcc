use crate::parse::StaticInit;

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

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Constant {
    Int(i32),
    Long(i64),
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.is_negative() {
            f.write_str("n")?
        }

        match self.abs() {
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
        Self::new_int(c)
    }
}

impl From<i64> for Constant {
    fn from(c: i64) -> Self {
        Self::new_long(c)
    }
}


impl Constant {
    pub const fn new_int(int:i32) -> Self{
        Self::Int(int)
    }

    pub const fn new_long(long:i64) -> Self{
        Self::Long(long)
    }

    pub fn map<T, F: Fn(i32) -> T, G: Fn(i64) -> T>(self, f:F, g:G) -> T{
        match self{
            Constant::Int(i) => f(i),
            Constant::Long(l) => g(l),
        }
    }

    pub fn edit<F: Fn(i32) -> i32, G: Fn(i64) -> i64>(self, f:F, g:G) -> Self{
        match self{
            Constant::Int(i) => Self::Int(f(i)),
            Constant::Long(l) => Self::Long(g(l)),
        }
    }


    const fn copied(&self) -> Self{
        *self
    }

    pub fn int(&self) -> i32 {
        self.copied().map(|i| i, |l| l as i32)
    }

    pub fn long(&self) -> i64 {
        self.copied().map(|i| i as i64, |l| l)
    }

    pub fn ty(&self) -> crate::VarType {
        use crate::VarType;
        self.copied().map(|_| VarType::Int, |_| VarType::Long)
    }

    pub fn abs(&self) -> Self {
        self.copied().edit(|i| i.abs(), |l| l.abs())
    }

    pub fn is_positive(&self) -> bool {
        self.copied().map(i32::is_positive, i64::is_positive)

    }

    pub fn is_negative(&self) -> bool {
        !self.is_positive()
    }

    pub fn with_unop(&self, op: crate::parse::UnOp) -> Self {
        use crate::parse::UnOp;
        match op {
            UnOp::Complement => self.edit(|i| !i, |l| !l),

            UnOp::Negate => self.edit(|i| -i, |l| -l),

            UnOp::Not => self.copied().edit(|i| if i == 0 {i} else {1}, |l| if l == 0 {l} else {1}),
        }
    }

    pub fn with_incdec(&self, op: crate::parse::IncDec) -> Self {
        use crate::parse::inc_dec::{POST_DEC, POST_INC, PRE_DEC, PRE_INC};
        let c = self.copied();
        match op {
            POST_INC | POST_DEC => c,
            PRE_INC => c.edit(|i| i.wrapping_add(1), |l| l.wrapping_add(1)),
            PRE_DEC => c.edit(|i| i.wrapping_sub(1), |l| l.wrapping_sub(1)),
        }
    }

    pub fn static_init(&self) -> StaticInit {
        self.copied().map(StaticInit::Int, StaticInit::Long)
    }

    pub fn eq(&self, int:i32, long:i64) -> bool{
        self.copied().map(|i| i == int, |l| l == long)
    }

    pub fn is_zero(&self) -> bool {
        self.eq(0,0)
    }
}

impl From<StaticInit> for Constant{
    fn from(value: StaticInit) -> Self {
        match value{
            StaticInit::Int(i) => Self::Int(i),
            StaticInit::Long(l) => Self::Long(l),
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
