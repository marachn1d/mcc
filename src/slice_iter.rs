use crate::lex::Token;
use crate::lex::{Constant, Identifier};
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

pub struct TokenIter(Vec<Token>);
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

use super::parse;
impl TokenIter {
    pub fn new(tokens: Box<[Token]>) -> Self {
        let mut tokens: Vec<Token> = tokens.into();
        tokens.reverse();
        Self(tokens)
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.0.last()
    }

    pub fn next_if(&mut self, f: impl Fn(&Token) -> bool) -> Option<Token> {
        let next = self.peek()?;
        if f(next) {
            self.next()
        } else {
            None
        }
    }
    pub fn next_if_map<Y>(&mut self, f: impl Fn(Token) -> Option<Y>) -> Option<Y> {
        f(self.next()?)
    }

    pub fn consume_array<const N: usize>(&mut self, slice: [Token; N]) -> Result<(), parse::Error> {
        for token in slice {
            if self.peek().is_some_and(|x| x == &token) {
                self.next();
            } else {
                return Err(parse::Error::Expected(token));
            }
        }

        Ok(())
    }

    pub fn peek_any(&self) -> Result<&Token, parse::Error> {
        self.peek().ok_or(parse::Error::UnexpectedEof)
    }

    pub fn peek_peek(&self) -> Option<&Token> {
        if self.0.len() <= 2 {
            None
        } else {
            self.0.get(self.0.len() - 2)
        }
    }

    pub fn consume_any(&mut self) -> Result<Token, parse::Error> {
        self.next().ok_or(parse::Error::UnexpectedEof)
    }

    pub fn consume(&mut self, token: impl Into<Token>) -> Result<(), parse::Error> {
        let token = token.into();
        if self.peek().is_some_and(|x| x == &token) {
            self.next();
        } else {
            return Err(parse::Error::Expected(token));
        }

        Ok(())
    }

    pub fn consume_identifier(&mut self) -> Result<Identifier, parse::Error> {
        match self.next_if(Token::identifier) {
            Some(Token::Identifier(ident)) => Ok(ident),
            None => Err(parse::Error::UnexpectedEof),
            _ => Err(parse::Error::ExpectedIdentifier),
        }
    }

    pub fn consume_constant(&mut self) -> Result<Constant, parse::Error> {
        match self.next_if(Token::constant) {
            Some(Token::Constant(c)) => Ok(c),
            None => Err(parse::Error::UnexpectedEof),
            _ => Err(parse::Error::ExpectedConstant),
        }
    }
}

impl Iterator for TokenIter {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.0.pop()
    }
}
