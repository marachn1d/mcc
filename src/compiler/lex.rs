use super::slice_iter::SliceIter;
use std::fmt::{self, Display, Formatter};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
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
    Asterisk,
    Slash,
    Percent,
}

pub fn tokenize(bytes: &[u8]) -> Result<Box<[Token]>, Error> {
    let mut iter = SliceIter::new(bytes);

    let mut tokens = Vec::new();
    while let Some(token) = lex_slice(&mut iter)? {
        tokens.push(token)
    }
    Ok(tokens.into())
}

fn lex_slice(iter: &mut SliceIter<u8>) -> Result<Option<Token>, Error> {
    match iter.as_slice() {
        [b'-', b'-', ..] => {
            iter.next();
            iter.next();
            Ok(Some(Token::Decrement))
        }
        [a, ..] if !a.is_ascii() => error("Invalid Character (I Only Accept Ascii :[)"),
        [a, ..] if a.is_ascii_whitespace() => {
            iter.next();
            lex_slice(iter)
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
                &a if AsciiDigit::from_int(a).is_some() => {
                    let byte = AsciiDigit::from_int(a).unwrap();
                    Token::Constant(constant_number(byte, iter)?)
                }
                b'-' => Token::Minus,
                b'+' => Token::Plus,
                b'*' => Token::Asterisk,
                b'/' => Token::Slash,
                b'%' => Token::Percent,
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
    if iter.peek().is_some_and(|byte| !word_character(byte)) {
        let number = parse_digit(&bytes);
        Ok(Constant::Integer(number))
    } else {
        Err(Error::InvalidConstant)
    }
}

fn literal(byte: u8, iter: &mut SliceIter<u8>) -> Result<Token, Error> {
    let mut bytes = vec![byte];
    while let Some(character) = next_if_word(iter) {
        bytes.push(character);
    }
    if iter.peek().is_some_and(|byte| !word_character(byte)) {
        use keywords::{INT, RETURN, VOID};
        Ok(match bytes.as_slice() {
            INT => Keyword::Int.into(),
            RETURN => Keyword::Return.into(),
            VOID => Keyword::Void.into(),
            _ => identifier(bytes.into())?.into(),
        })
    } else {
        Err(Error::InvalidLiteral)
    }
}

fn identifier(bytes: Box<[u8]>) -> Result<Identifier, Error> {
    if bytes[0].is_ascii_alphanumeric() && bytes[1..].iter().all(|&x| word_character(x)) {
        Ok(Identifier(bytes))
    } else {
        Err(Error::InvalidIdentifier)
    }
}

const fn _word_boundary(byte: u8) -> bool {
    !word_character(byte)
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

fn parse_digit(slice: &[AsciiDigit]) -> u64 {
    let mut cur = 0u64;
    for (place, digit) in slice.iter().map(|&x| x as u8 as u64).rev().enumerate() {
        cur += 10u64.pow(place as u32) * digit
    }
    cur
}

fn next_if_word(iter: &mut SliceIter<u8>) -> Option<u8> {
    iter.next_if(word_character)
}

impl PartialEq<Token> for Keyword {
    fn eq(&self, other: &Token) -> bool {
        if let Token::Keyword(k) = other {
            k == self
        } else {
            false
        }
    }
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier(pub Box<[u8]>);

impl AsRef<[u8]> for Identifier {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl From<Identifier> for Token {
    fn from(i: Identifier) -> Self {
        Self::Identifier(i)
    }
}

impl From<Keyword> for Token {
    fn from(k: Keyword) -> Self {
        Self::Keyword(k)
    }
}

impl From<Constant> for Token {
    fn from(c: Constant) -> Self {
        Self::Constant(c)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

mod keywords {
    pub const INT: &[u8] = b"int";
    pub const VOID: &[u8] = b"void";
    pub const RETURN: &[u8] = b"return";
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Constant {
    Integer(u64),
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
