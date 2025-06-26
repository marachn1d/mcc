use ast::Constant;
use ast::Ident;
use ast::Token;
use ast::VarType;
use std::iter::Iterator;
use std::slice::Iter;
pub struct SliceIter<'a, T: Copy>(Iter<'a, T>);

impl<T: Copy> Iterator for SliceIter<'_, T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.0.next().copied()
    }
}

impl<'a, T: Copy> SliceIter<'a, T> {
    pub fn new(slice: &'a [T]) -> Self {
        Self(slice.iter())
    }

    pub fn peek(&self) -> Option<T> {
        self.as_slice().first().copied()
    }

    pub fn as_slice(&self) -> &'a [T] {
        self.0.as_slice()
    }

    pub fn next_if(&mut self, f: impl Fn(T) -> bool) -> Option<T> {
        let next = self.peek()?;
        if f(next) {
            self.next()
        } else {
            None
        }
    }

    pub fn next_if_map<Y>(&mut self, f: impl Fn(T) -> Option<Y>) -> Option<Y> {
        let next = self.peek()?;
        let res = f(next);
        if res.is_some() {
            self.next();
        }
        res
    }
}

pub struct TokenIter(std::vec::IntoIter<Token>);

use fmt::Debug;
use fmt::Formatter;
use std::fmt;
impl Debug for TokenIter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.peek() {
            Some(t) => write!(f, "TokenIter{{{t:?}}}"),
            None => write!(f, "TokenIter{{_}}"),
        }
    }
}
//use super::parse;
use thiserror::Error;
#[derive(Error, Debug)]
pub enum Expected {
    #[error("Unexpected Eof")]
    Eof,
    #[error("Expected Constant, got {0}")]
    Constant(Token),
    #[error("Expected Identifier, got {0}")]
    Identifier(Token),
    #[error("Expected Token {expected}, got {got}")]
    Token { expected: Token, got: Token },

    #[error("Expected Type, got {0}")]
    Type(Token),
}

fn expected<T>(tok: Option<Token>, exp: impl FnOnce(Token) -> Expected) -> Result<T, Expected> {
    match tok {
        Some(x) => Err(exp(x)),
        None => Err(Expected::Eof),
    }
}

impl TokenIter {
    #[allow(dead_code)]
    pub fn print_next(&self) {
        eprintln!("next: {:?}", self.peek());
    }

    pub fn consume_type(&mut self) -> Result<VarType, Expected> {
        match self.next() {
            Some(Token::Int) => Ok(VarType::Int),
            Some(Token::Long) => Ok(VarType::Long),
            a => expected(a, Expected::Type),
        }
    }

    pub fn new(tokens: Box<[Token]>) -> Self {
        let tokens: Vec<Token> = tokens.into();
        Self(tokens.into_iter())
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.0.as_slice().first()
    }

    pub fn next_if(&mut self, f: impl Fn(&Token) -> bool) -> Option<Token> {
        let next = self.peek()?;
        if f(next) {
            self.next()
        } else {
            None
        }
    }
    pub fn next_if_map<T>(&mut self, f: impl Fn(Token) -> Option<T>) -> Option<T> {
        f(self.next()?)
    }

    pub fn peek_any(&self) -> Result<&Token, Expected> {
        self.peek().ok_or(Expected::Eof)
    }

    pub fn as_slice(&self) -> &[Token] {
        self.0.as_slice()
    }

    pub fn peek_peek(&self) -> Option<&Token> {
        self.0.as_slice().get(1)
    }

    pub fn consume(&mut self, token: impl Into<Token>) -> Result<(), Expected> {
        let expected = token.into();
        let got = self.peek_any()?;
        if got == &expected {
            Ok(())
        } else {
            Err(Expected::Token {
                expected,
                got: got.clone(),
            })
        }
    }

    pub fn consume_arr(&mut self, iter: impl IntoIterator<Item = Token>) -> Result<(), Expected> {
        for token in iter {
            self.consume(token)?;
        }
        Ok(())
    }

    pub fn consume_identifier(&mut self) -> Result<Ident, Expected> {
        match self.next() {
            Some(Token::Ident(ident)) => Ok(ident),
            other => expected(other, Expected::Identifier),
        }
    }

    pub fn consume_constant(&mut self) -> Result<Constant, Expected> {
        match self.next_if(Token::constant) {
            Some(Token::Const(c)) => Ok(c),
            other => expected(other, Expected::Constant),
        }
    }

    pub fn consume_any(&mut self) -> Result<Token, Expected> {
        self.next().ok_or(Expected::Eof)
    }
    pub fn token_slice(&self) -> &[Token] {
        self.as_slice()
    }

    pub fn take_until<T: From<Expected>>(
        &mut self,
        mut f: impl FnMut(&Token) -> Result<bool, T>,
    ) -> Result<(), T> {
        let mut next = self.peek_any()?;

        while f(next)? {
            self.next();
            next = self.peek_any()?;
        }
        Ok(())
    }
}

impl Iterator for TokenIter {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.0.next()
    }
}
