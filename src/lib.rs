use std::fs;
use std::io;
use std::path::PathBuf;

pub mod codegen;
pub mod lex;

pub mod parse;
pub mod semantics;
pub mod slice_iter;
pub use lex::Token;
use std::sync::OnceLock;
pub static CONFIG: OnceLock<Config> = OnceLock::new();
// todo: make config some sorta static thats initialized on startup etc
pub struct Config {
    pub stage: Option<CompileStage>,
    pub version: CVersion,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CVersion {
    C17,
    C23,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, clap::ValueEnum)]
pub enum CompileStage {
    Lex,
    Parse,
    Codegen,
    #[value(name = "S")]
    KeepAsm,
    Compile,
    Tacky,
    Validate,
}

pub fn compile(mut path: PathBuf) -> Result<PathBuf, Error> {
    let bytes = fs::read(&path).map_err(|_| Error::InvalidInput)?;
    let _ = fs::remove_file(&path);
    let stage = CONFIG.get().unwrap().stage;

    let tokens = lex::tokenize(&bytes)?;
    if stage == Some(CompileStage::Lex) {
        return Ok("".into());
    }
    let program = parse::parse(tokens)?;
    if stage == Some(CompileStage::Parse) {
        return Ok("".into());
    }

    let program = semantics::check(program)?;
    if stage == Some(CompileStage::Validate) {
        return Ok("".into());
    }

    let code = codegen::generate(program, stage != Some(CompileStage::Tacky));

    if matches!(stage, Some(CompileStage::Codegen | CompileStage::Tacky)) {
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
    Semantics(semantics::Error),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<semantics::Error> for Error {
    fn from(e: semantics::Error) -> Self {
        Error::Semantics(e)
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
