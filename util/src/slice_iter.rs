use ast::{Constant, Token};
use std::iter::Iterator;
use std::slice::Iter;
use std::str::Chars;
use symtab::Key;

pub struct SliceIter<'a>(Chars<'a>);

impl Iterator for SliceIter<'_> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        self.0.next()
    }
}

impl<'a> SliceIter<'a> {
    pub fn new(slice: &'a str) -> Self {
        Self(slice.chars())
    }

    pub fn peek(&self) -> Option<char> {
        self.as_slice().chars().next()
    }

    pub fn as_slice(&self) -> &'a str {
        self.0.as_str()
    }

    pub fn next_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let next = self.peek()?;
        if f(next) {
            self.next()
        } else {
            None
        }
    }

    pub fn next_if_map<T>(&mut self, f: impl Fn(char) -> Option<T>) -> Option<T> {
        let next = self.peek()?;
        let res = f(next);
        if res.is_some() {
            self.next();
        }
        res
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.as_slice().as_bytes()
    }
}

pub struct TokenIter<'a>(std::slice::Iter<'a, Token<'a>>);

use fmt::Debug;
use fmt::Formatter;
use std::fmt;
impl Debug for TokenIter<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.peek() {
            Some(t) => write!(f, "TokenIter{{{t:?}}}"),
            None => write!(f, "TokenIter{{_}}"),
        }
    }
}

use ast::types_prelude::*;

#[derive(Debug, thiserror::Error)]
pub enum TokenIterError<'a> {
    #[error("Expected Eof")]
    UnexpectedEof,
    #[error("Expected Constant, Got {0}")]
    ExpectedConstantGot(Token<'a>),
    #[error("Expected Identifier, Got {0}")]
    ExpectedIdentifierGot(Token<'a>),
    #[error("Expected {wanted}, Got {got}")]
    Expected { wanted: Token<'a>, got: Token<'a> },
}

// b is the lifetime of the borrow
impl<'b, 'a: 'b> TokenIter<'a> {
    #[allow(dead_code)]
    pub fn print_next(&self) {
        eprintln!("next: {:?}", self.peek());
    }

    pub fn consume_type(&mut self) -> Option<VarType> {
        match self.next_if(|x| x == &Token::Int || x == &Token::Long) {
            Some(Token::Int) => Some(VarType::Int),
            Some(Token::Long) => Some(VarType::Long),
            _ => None,
        }
    }

    pub fn new(tokens: &'a [Token]) -> Self {
        Self(tokens.iter())
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    pub fn peek(&self) -> Option<&Token<'a>> {
        self.0.as_slice().first()
    }

    pub fn next_if(&mut self, f: impl Fn(&Token<'a>) -> bool) -> Option<Token<'a>> {
        if let Some(next) = self.peek() {
            if f(next) {
                self.next()
            } else {
                None
            }
        } else {
            None
        }
    }
    pub fn next_if_map<T>(&mut self, f: impl Fn(Token) -> Option<T>) -> Option<T> {
        f(self.next()?)
    }

    pub fn peek_any(&self) -> Result<&Token<'a>, TokenIterError<'a>> {
        self.peek().ok_or(TokenIterError::UnexpectedEof)
    }

    pub fn as_slice(&self) -> &[Token] {
        self.0.as_slice()
    }

    pub fn peek_peek(&self) -> Option<&Token> {
        self.0.as_slice().get(1)
    }

    pub fn consume(
        &'b mut self,
        token: impl Into<Token<'static>>,
    ) -> Result<Option<Token<'a>>, TokenIterError<'a>> {
        let token = token.into();
        match self.peek() {
            Some(t) if *t == token => Ok(self.next()),
            Some(got) => Err(TokenIterError::Expected {
                wanted: token,
                got: *got,
            }),
            None => Err(TokenIterError::UnexpectedEof),
        }
    }

    pub fn consume_arr(
        &'b mut self,
        iter: impl IntoIterator<Item = Token<'static>>,
    ) -> Result<(), TokenIterError<'b>> {
        for token in iter {
            self.consume(token)?;
        }
        Ok(())
    }

    pub fn consume_identifier(&'b mut self) -> Result<Key<'a>, TokenIterError<'a>> {
        match self.next_if(Token::identifier) {
            Some(Token::Ident(ident)) => Ok(ident),
            Some(t) => Err(TokenIterError::ExpectedIdentifierGot(t)),
            None => Err(TokenIterError::UnexpectedEof),
        }
    }

    pub fn consume_constant(&'a mut self) -> Result<Constant, TokenIterError<'a>> {
        match self.next_constant()? {
            either::Either::Left(c) => Ok(c),
            either::Either::Right(g) => Err(TokenIterError::ExpectedConstantGot(g)),
        }
    }

    fn next_constant(
        &'b mut self,
    ) -> Result<either::Either<Constant, Token<'a>>, TokenIterError<'a>> {
        let next = self.peek_any()?;
        Ok(if let Token::Const(c) = *next {
            either::Either::Left(c)
        } else {
            either::Either::Right(*next)
        })
    }

    pub fn consume_any(&mut self) -> Result<Token<'a>, TokenIterError<'a>> {
        self.next().ok_or(TokenIterError::UnexpectedEof)
    }
    pub fn token_slice(&self) -> &[Token] {
        self.as_slice()
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Token<'a>> {
        self.0.next().copied()
    }
}
