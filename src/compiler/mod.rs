use crate::Arg;
use std::fs;
use std::io;
use std::path::PathBuf;

pub mod codegen;
pub mod lex;
pub mod parse;

pub use lex::Token;

pub fn compile(mut path: PathBuf, arg: Option<Arg>) -> Result<PathBuf, Error> {
    let bytes = fs::read(&path).map_err(|_| Error::InvalidInput)?;
    let _ = fs::remove_file(&path);
    let tokens = lex::tokenize(&bytes)?;
    if arg == Some(Arg::Lex) {
        return Ok("".into());
    }
    let program = parse::parse(&tokens)?;
    if arg == Some(Arg::Parse) {
        return Ok("".into());
    }
    let code = codegen::generate(program, arg != Some(Arg::Tacky));

    if matches!(arg, Some(Arg::Codegen) | Some(Arg::Tacky)) {
        return Ok("".into());
    }

    path.set_extension("S");
    fs::write(&path, &code)?;
    Ok(path)
}

#[derive(Debug)]
pub enum Error {
    Todo,
    InvalidInput,
    Io(io::Error),
    Lexing(lex::Error),
    Parsing(parse::Error),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<lex::Error> for Error {
    fn from(e: lex::Error) -> Self {
        Error::Lexing(e)
    }
}

impl From<parse::Error> for Error {
    fn from(e: parse::Error) -> Self {
        Error::Parsing(e)
    }
}
