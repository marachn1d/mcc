use crate::compiler::lex::Token;
use std::iter::Iterator;
use std::slice::Iter;
use std::vec::IntoIter;
pub struct SliceIter<'a, T: Copy>(Iter<'a, T>);

impl<'a, T: Copy> Iterator for SliceIter<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.0.next().copied()
    }
}

impl<'a, T: PartialEq + Copy> SliceIter<'a, T> {
    pub fn next_if_eq(&mut self, value: impl PartialEq<T>) -> Option<T> {
        self.next_if(|x| value == x)
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

use super::parse;
impl TokenIter {
    pub fn new(tokens: Box<[Token]>) -> Self {
        let mut tokens: Vec<Token> = tokens.into();
        tokens.reverse();
        Self(tokens)
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_some()
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
}

impl Iterator for TokenIter {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.0.pop()
    }
}
