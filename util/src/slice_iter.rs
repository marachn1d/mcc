use ast::Constant;
use ast::Token;
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

pub struct TokenIter<T>(std::vec::IntoIter<Token<T>>);

use fmt::Debug;
use fmt::Formatter;
use std::fmt;
impl<T: Debug> Debug for TokenIter<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.peek() {
            Some(t) => write!(f, "TokenIter{{{t:?}}}"),
            None => write!(f, "TokenIter{{_}}"),
        }
    }
}
//use super::parse;
//use parse::VarType;

impl<T> TokenIter<T> {
    /*
    #[allow(dead_code)]
    pub fn print_next(&self) {
        eprintln!("next: {:?}", self.peek());
    }
    */

    /*
    pub fn consume_type(&mut self) -> Option<VarType> {
        match self.next_if(|x| x == &Token::Int || x == &Token::Long) {
            Some(Token::Int) => Some(VarType::Int),
            Some(Token::Long) => Some(VarType::Long),
            _ => None,
        }
    }
    */

    pub fn new(tokens: Box<[Token<T>]>) -> Self {
        let tokens: Vec<Token<T>> = tokens.into();
        Self(tokens.into_iter())
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    pub fn peek(&self) -> Option<&Token<T>> {
        self.0.as_slice().first()
    }

    pub fn next_if(&mut self, f: impl Fn(&Token<T>) -> bool) -> Option<Token<T>> {
        let next = self.peek()?;
        if f(next) {
            self.next()
        } else {
            None
        }
    }
    pub fn next_if_map<U>(&mut self, f: impl Fn(Token<T>) -> Option<T>) -> Option<T> {
        f(self.next()?)
    }

    /*
    pub fn peek_any(&self) -> Result<&Token<T>, parse::Error> {
        self.peek().ok_or(parse::Error::UnexpectedEof)
    }
    */

    pub fn as_slice(&self) -> &[Token<T>] {
        self.0.as_slice()
    }

    pub fn peek_peek(&self) -> Option<&Token<T>> {
        self.0.as_slice().get(1)
    }

    /*
    pub fn consume(&mut self, token: impl Into<Token<T>>) -> Result<(), parse::Error> {
        let token = token.into();
        if self.peek().is_some_and(|x| x == &token) {
            self.next();
        } else {
            return Err(parse::Error::Expected(token));
        }

        Ok(())
    }

    pub fn consume_arr(
        &mut self,
        iter: impl IntoIterator<Item = Token>,
    ) -> Result<(), parse::Error> {
        for token in iter {
            self.consume(token)?;
        }
        Ok(())
    }

    pub fn consume_identifier(&mut self) -> Result<Identifier, parse::Error> {
        match self.next_if(Token::identifier) {
            Some(Token::Identifier(ident)) => Ok(ident),
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

    pub fn consume_any(&mut self) -> Result<Token, parse::Error> {
        self.next().ok_or(parse::Error::UnexpectedEof)
    }
    pub fn token_slice(&self) -> &[Token] {
        self.as_slice()
    }

    pub fn take_until(
        &mut self,
        mut f: impl FnMut(&Token) -> Result<bool, parse::Error>,
    ) -> Result<(), parse::Error> {
        let mut next = self.peek_any()?;

        while f(next)? {
            self.next();
            next = self.peek_any()?;
        }
        Ok(())
    }
    */
}

impl<T> Iterator for TokenIter<T> {
    type Item = Token<T>;
    fn next(&mut self) -> Option<Token<T>> {
        self.0.next()
    }
}
