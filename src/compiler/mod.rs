use crate::Arg;
use std::fs;
use std::io;
use std::path::PathBuf;

pub mod codegen;
pub mod lex;
pub mod parse;

pub fn compile(mut path: PathBuf, arg: Option<Arg>) -> Result<PathBuf, Error> {
    let bytes = fs::read(&path).map_err(|_| Error::InvalidInput)?;
    let _tokens = lex::tokenize(&bytes);
    // do stuff
    let _ = fs::remove_file(&path);

    Err(Error::Todo)
}

#[derive(Debug)]
pub enum Error {
    Todo,
    InvalidInput,
    Io(io::Error),
    Lexing(lex::Error),
}
