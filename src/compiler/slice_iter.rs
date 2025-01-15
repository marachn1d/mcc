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
