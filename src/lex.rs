use super::slice_iter::SliceIter;

use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct DebugToken {
    pub token: Token,
    pub line: usize,
}

impl DebugToken {
    pub fn into_inner(self) -> (Token, usize) {
        (self.token, self.line)
    }

    pub const fn line(&self) -> usize {
        self.line
    }
}

impl std::ops::Deref for DebugToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}
use std::ops::Deref;

use std::ops::DerefMut;
impl AsRef<Token> for DebugToken {
    fn as_ref(&self) -> &Token {
        self.deref()
    }
}

impl AsMut<Token> for DebugToken {
    fn as_mut(&mut self) -> &mut Token {
        self.deref_mut()
    }
}

impl std::ops::DerefMut for DebugToken {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

impl From<DebugToken> for Token {
    fn from(debug: DebugToken) -> Token {
        debug.token
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
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

    Constant(Constant),
    Identifier(Identifier),
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

pub fn tokenize(bytes: &[u8]) -> Result<Box<[DebugToken]>, Error> {
    let mut iter = SliceIter::new(bytes);

    let mut tokens = Vec::new();
    let mut cur_line = 0;
    while let Some(token) = lex_slice(&mut iter, &mut cur_line)? {
        tokens.push(DebugToken {
            token,
            line: cur_line,
        });
    }
    Ok(tokens.into())
}

fn lex_slice(iter: &mut SliceIter<u8>, cur_line: &mut usize) -> Result<Option<Token>, Error> {
    match iter.as_slice() {
        [b'<', b'<', b'=', ..] => {
            iter.next();
            iter.next();
            iter.next();
            Ok(Some(Token::LeftShiftEqual))
        }
        [b'>', b'>', b'=', ..] => {
            iter.next();
            iter.next();
            iter.next();
            Ok(Some(Token::RightShiftEqual))
        }
        [b'-', b'-', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Decrement))
        }
        [b'<', b'<', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::LeftShift))
        }
        [b'&', b'&', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::LogicalAnd))
        }
        [b'|', b'|', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::LogicalOr))
        }
        [b'!', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::NotEqual))
        }
        [b'=', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::EqualTo))
        }
        [b'>', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Geq))
        }
        [b'<', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Leq))
        }
        [b'>', b'>', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::RightShift))
        }
        [b'+', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::PlusEqual))
        }
        [b'+', b'+', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Increment))
        }
        [b'-', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::MinusEqual))
        }
        [b'*', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::TimesEqual))
        }
        [b'/', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::DivEqual))
        }
        [b'%', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::PercentEqual))
        }
        [b'&', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::BitAndEqual))
        }
        [b'|', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::BitOrEqual))
        }
        [b'^', b'=', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::BitXorEqual))
        }

        [a, ..] if !a.is_ascii() => error("Invalid Character (I Only Accept Ascii :[)"),
        [a, ..] if a.is_ascii_whitespace() => {
            if *a == b'\n' {
                *cur_line += 1;
            }
            iter.next();
            lex_slice(iter, cur_line)
        }
        [a, ..] => {
            iter.next();
            Ok(Some(match a {
                b'(' => Token::OpenParen,
                b')' => Token::CloseParen,
                b'{' => Token::OpenBrace,
                b';' => Token::Semicolon,
                b'}' => Token::CloseBrace,
                b'~' => Token::Tilde,
                b'0'..=b'9' => {
                    let byte = AsciiDigit::from_int(*a).unwrap();
                    Token::Constant(constant_number(byte, iter)?)
                }
                b'-' => Token::Minus,
                b'+' => Token::Plus,
                b'*' => Token::Asterisk,
                b'/' => Token::Slash,
                b'%' => Token::Percent,
                b'&' => Token::Ampersand,
                b'|' => Token::Bar,
                b'^' => Token::Caret,
                b'!' => Token::Not,
                b'<' => Token::LessThan,
                b'>' => Token::GreaterThan,
                b'=' => Token::Equals,
                b',' => Token::Comma,
                b'?' => Token::QuestionMark,
                b':' => Token::Colon,
                a => literal(*a, iter)?,
            }))
        }
        [] => Ok(None),
    }
}

impl AsciiDigit {
    const fn from_int(int: u8) -> Option<Self> {
        match int {
            b'0' => Some(AsciiDigit::Zero),
            b'1' => Some(AsciiDigit::One),
            b'2' => Some(AsciiDigit::Two),
            b'3' => Some(AsciiDigit::Three),
            b'4' => Some(AsciiDigit::Four),
            b'5' => Some(AsciiDigit::Five),
            b'6' => Some(AsciiDigit::Six),
            b'7' => Some(AsciiDigit::Seven),
            b'8' => Some(AsciiDigit::Eight),
            b'9' => Some(AsciiDigit::Nine),
            _ => None,
        }
    }
}

fn constant_number(start: AsciiDigit, iter: &mut SliceIter<u8>) -> Result<Constant, Error> {
    let mut bytes = vec![start];
    while let Some(constant) = next_if_number(iter) {
        bytes.push(constant);
    }

    match iter.peek() {
        Some(b'l') => {
            iter.next();
            Ok(Constant::from(parse_long(&bytes)))
        }
        Some(x) if !word_character(x) => {
            let long = parse_long(&bytes);
            i32::try_from(long)
                .map(Constant::Int)
                .or(Ok(Constant::Long(long)))
        }
        _ => Err(Error::InvalidConstant),
    }
}

fn literal(byte: u8, iter: &mut SliceIter<u8>) -> Result<Token, Error> {
    let mut bytes = vec![byte];
    while let Some(character) = next_if_word(iter) {
        bytes.push(character);
    }
    if iter.peek().is_some_and(|byte| !word_character(byte)) {
        Ok(match bytes.as_slice() {
            b"int" => Token::Int,
            b"return" => Token::Return,
            b"void" => Token::Void,
            b"if" => Token::If,
            b"else" => Token::Else,
            b"goto" => Token::Goto,
            b"do" => Token::Do,
            b"while" => Token::While,
            b"for" => Token::For,
            b"break" => Token::Break,
            b"continue" => Token::Continue,
            b"switch" => Token::Switch,
            b"case" => Token::Case,
            b"default" => Token::Default,
            b"static" => Token::Static,
            b"extern" => Token::Extern,
            b"long" => Token::Long,
            _ => identifier(bytes.into())?.into(),
        })
    } else {
        Err(Error::InvalidLiteral)
    }
}

fn identifier(bytes: Box<[u8]>) -> Result<Identifier, Error> {
    if word_start(bytes[0]) && bytes[1..].iter().all(|&x| word_character(x)) {
        Ok(Identifier(bytes.into()))
    } else {
        Err(Error::InvalidIdentifier)
    }
}

const fn _word_boundary(byte: u8) -> bool {
    !word_character(byte)
}

const fn word_start(byte: u8) -> bool {
    match byte {
        b if b.is_ascii_alphabetic() => true,
        b'_' => true,
        _ => false,
    }
}

const fn word_character(byte: u8) -> bool {
    match byte {
        b if b.is_ascii_alphanumeric() => true,
        b'_' => true,
        _ => false,
    }
}

fn next_if_number(iter: &mut SliceIter<u8>) -> Option<AsciiDigit> {
    iter.next_if_map(AsciiDigit::from_int)
}

#[derive(Clone, Copy)]
enum AsciiDigit {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
}

fn parse_long(slice: &[AsciiDigit]) -> i64 {
    let mut cur = 0i64;
    for (place, digit) in slice.iter().map(|&x| i64::from(x as u8)).rev().enumerate() {
        cur += 10i64.pow(place as u32) * digit;
    }
    cur
}
impl Token {
    pub const fn identifier(&self) -> bool {
        matches!(self, Self::Identifier(_))
    }
    pub const fn constant(&self) -> bool {
        matches!(self, Self::Constant(_))
    }
}

fn next_if_word(iter: &mut SliceIter<u8>) -> Option<u8> {
    iter.next_if(word_character)
}

impl PartialEq<Token> for Constant {
    fn eq(&self, other: &Token) -> bool {
        if let Token::Constant(c) = other {
            c == self
        } else {
            false
        }
    }
}

impl PartialEq<Token> for Identifier {
    fn eq(&self, other: &Token) -> bool {
        if let Token::Identifier(c) = other {
            c == self
        } else {
            false
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Identifier(pub Rc<Box<[u8]>>);

impl Identifier {
    pub fn new(name: &[u8]) -> Self {
        let mut vec = Vec::new();
        vec.extend_from_slice(name);
        Self(Rc::new(vec.into_boxed_slice()))
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", unsafe { std::str::from_utf8_unchecked(&self.0) })
    }
}

impl AsRef<[u8]> for Identifier {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Self {
        Self(s.into_bytes().into_boxed_slice().into())
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        let r#box: Box<[u8]> = s.as_bytes().iter().copied().collect();
        Self(r#box.into())
    }
}

impl From<&[u8]> for Identifier {
    fn from(s: &[u8]) -> Self {
        let r#box: Box<[u8]> = s.iter().copied().collect();
        Self(r#box.into())
    }
}

impl From<Identifier> for Token {
    fn from(i: Identifier) -> Self {
        Self::Identifier(i)
    }
}

impl From<Constant> for Token {
    fn from(c: Constant) -> Self {
        Self::Constant(c)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
pub enum Constant {
    Int(i32),
    Long(i64),
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

impl From<i32> for Constant {
    fn from(i: i32) -> Self {
        Self::Int(i)
    }
}

impl From<i64> for Constant {
    fn from(i: i64) -> Self {
        Self::Long(i)
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Int(i) => i.fmt(f),
            Self::Long(l) => l.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidConstant,
    InvalidLiteral,
    InvalidIdentifier,
    NotAscii,
    Other(String),
}

fn error<T>(message: &str) -> Result<T, Error> {
    Err(Error::Other(message.into()))
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use std::str::from_utf8_unchecked;
        write!(f, "{}", unsafe { from_utf8_unchecked(&self.0) })
    }
}
