//pub mod ast;

use std::fs;
use std::io;
use std::path::PathBuf;

pub mod lex;
pub use lex::DebugToken;
pub use lex::Token;
pub mod slice_iter;

#[cfg(feature = "codegen")]
pub mod codegen;
#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "semantics")]
pub mod semantics;

use std::sync::OnceLock;
pub static CONFIG: OnceLock<Config> = OnceLock::new();
// todo: make config some sorta static thats initialized on startup etc
pub struct Config {
    pub stage: Option<CompileStage>,
    pub version: CVersion,
}

pub fn compile(mut path: PathBuf) -> Result<PathBuf, Error> {
    let bytes = fs::read(&path).map_err(|_| Error::InvalidInput)?;
    let _ = fs::remove_file(&path);
    let stage = CONFIG.get().unwrap().stage;

    let tokens = lex::tokenize(&bytes)?;
    if stage == Some(CompileStage::Lex) {
        return Ok("".into());
    }

    #[cfg(feature = "parse")]
    let code = parse(tokens, stage)?;

    if matches!(stage, Some(CompileStage::Codegen | CompileStage::Tacky)) {
        return Ok("".into());
    }
    #[cfg(feature = "codegen")]
    {
        path.set_extension("S");
        fs::write(&path, &code)?;
    }
    Ok(path)
}

fn parse(tokens: Box<[DebugToken]>, stage: Option<CompileStage>) -> Result<Box<[u8]>, Error> {
    let tokens = tokens.into_iter().map(|x| x.token).collect();
    let program = parse::parse(tokens)?;
    #[cfg(feature = "semantics")]
    {
        semantics(program, stage)
    }

    #[cfg(not(feature = "semantics"))]
    Ok([].into())
}

#[cfg(feature = "semantics")]
fn semantics(program: parse::Program, stage: Option<CompileStage>) -> Result<Box<[u8]>, Error> {
    let (program, symbol_table) = semantics::check(program)?;
    #[cfg(feature = "codegen")]
    {
        Ok(codegen(program, stage, symbol_table))
    }

    #[cfg(not(feature = "codegen"))]
    Ok([].into())
}

#[cfg(feature = "codegen")]
fn codegen(
    program: semantics::typed::Program,
    stage: Option<CompileStage>,
    table: semantics::SymbolTable,
) -> Box<[u8]> {
    codegen::generate(program, stage != Some(CompileStage::Tacky), table)
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CVersion {
    C17,
    C23,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum CompileStage {
    Lex,

    Parse,
    Codegen,

    Compile,

    Tacky,

    Validate,
}

#[derive(Debug)]
pub enum Error {
    Todo,
    InvalidInput,
    Io(io::Error),
    Lexing(lex::Error),

    #[cfg(feature = "parse")]
    Parsing(parse::Error),

    #[cfg(feature = "semantics")]
    Semantics(semantics::Error),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

#[cfg(feature = "semantics")]
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

#[cfg(feature = "parse")]
impl From<parse::Error> for Error {
    fn from(e: parse::Error) -> Self {
        Error::Parsing(e)
    }
}
