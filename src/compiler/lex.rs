use std::slice::Iter;

pub fn tokenize(bytes: &[u8]) -> Result<Box<[Token]>, Error> {
    let mut iter = bytes.iter();

    let mut tokens = Vec::new();
    while let Some(byte) = iter.next() {
        let token = match byte {
            b'(' => Token::OpenParen,
            b')' => Token::CloseParen,
            b'{' => Token::OpenBrace,
            b';' => Token::Semicolon,
            b'}' => Token::CloseBrace,
            a @ b'0'..=b'9' => lex_constant(*a, &mut iter)?.into(),
            a => literal(*a, &mut iter)?,
        };
        tokens.push(token);
    }

    Ok(tokens.into())
}

fn lex_constant(start: u8, iter: &mut Iter<u8>) -> Result<Constant, Error> {
    let mut bytes = vec![start];
    while let Some(constant) = next_if_number(iter) {
        bytes.push(constant);
    }
    if peek(iter).is_some_and(|byte| !word_character(byte)) {
        Ok(Constant::Integer(bytes.into()))
    } else {
        Err(Error::InvalidConstant)
    }
}

fn literal(byte: u8, iter: &mut Iter<u8>) -> Result<Token, Error> {
    let mut bytes = vec![byte];
    while let Some(character) = next_if_word(iter) {
        bytes.push(character);
    }
    if peek(iter).is_some_and(|byte| !word_character(byte)) {
        use keywords::{INT, RETURN, VOID};
        Ok(match bytes.as_slice() {
            INT => Keyword::Int.into(),
            RETURN => Keyword::Return.into(),
            VOID => Keyword::Void.into(),
            _ => Token::Identifier(bytes.into()),
        })
    } else {
        Err(Error::InvalidLiteral)
    }
}

const fn word_character(byte: u8) -> bool {
    byte.is_ascii_alphanumeric()
}

fn peek(iter: &Iter<u8>) -> Option<u8> {
    iter.as_slice().first().copied()
}

fn next_if_number(iter: &mut Iter<u8>) -> Option<u8> {
    next_if(iter, |x| x.is_ascii_digit())
}

fn next_if_word(iter: &mut Iter<u8>) -> Option<u8> {
    next_if(iter, word_character)
}

fn next_if(iter: &mut Iter<u8>, f: impl Fn(u8) -> bool) -> Option<u8> {
    let next = peek(iter)?;
    if f(next) {
        iter.next().copied()
    } else {
        None
    }
}

fn next_if_eq(iter: &mut Iter<u8>, target: u8) -> Option<u8> {
    next_if(iter, |x| x == target)
}

pub enum Token {
    Keyword(Keyword),
    Constant(Constant),
    Identifier(Box<[u8]>),
    OpenParen,
    CloseParen,
    OpenBrace,
    Semicolon,
    CloseBrace,
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

pub enum Constant {
    Integer(Box<[u8]>),
}

#[derive(Debug)]
pub enum Error {
    InvalidConstant,
    InvalidLiteral,
}
