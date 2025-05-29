//pub mod ast;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use symtab::{NotAscii, Store};
use thiserror::Error;
pub mod lex;
pub use ast::DebugToken;
pub use ast::Token;

#[cfg(feature = "codegen")]
pub mod codegen;
#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "semantics")]
pub mod semantics;

use std::sync::OnceLock;
pub static CONFIG: OnceLock<Config> = OnceLock::new();
#[derive(Copy, Clone, Debug)]
pub struct Config {
    pub stage: Option<CompileStage>,
    pub version: CVersion,
}

pub fn compile(path: &Path) -> Result<Option<PathBuf>, Error> {
    let mut bytes = Store::try_from(fs::read(path)?)?;
    let stage = CONFIG.get().unwrap().stage;

    let tokens = lex::tokenize(&bytes)?;
    let tokens: Box<[Token]> = tokens.into_iter().map(|x| x.token).collect();
    if stage == Some(CompileStage::Lex) {
        return Ok(None);
    }

    #[cfg(feature = "parse")]
    let ast = match parse(&tokens) {
        None => return Ok(None),
        Some(Err(e)) => return Err(e),
        Some(Ok(ast)) => ast,
    };

    #[cfg(feature = "semantics")]
    let (program, map) = if should_validate(&stage) {
        semantics::check(ast, &bytes)?
    } else {
        return Ok(None);
    };

    #[cfg(feature = "codegen")]
    if should_codegen(&stage) {
        let code = codegen::generate(&program, should_emit(&stage), map);
        let path = path.with_extension("S");
        fs::write(&path, &code)?;
        Ok(Some(path))
    } else {
        Ok(None)
    }

    #[cfg(not(feature = "codegen"))]
    Ok(None)
}

#[cfg(feature = "parse")]
fn parse<'a>(tokens: &'a [Token<'a>]) -> Option<Result<ast::parse::Program<'a>, Error>> {
    if should_parse(&CONFIG.get().unwrap().stage) {
        Some(parse::parse(tokens).map_err(Error::from))
    } else {
        None
    }
}

pub fn configure(config: &Config) {
    CONFIG
        .set(*config)
        .expect("error in configuration control flow")
}

#[cfg(feature = "codegen")]
const fn should_emit(s: &Option<CompileStage>) -> bool {
    should_codegen(s)
}
const fn should_parse(s: &Option<CompileStage>) -> bool {
    !matches!(s, Some(CompileStage::Lex))
}

#[cfg(feature = "semantics")]
const fn should_validate(s: &Option<CompileStage>) -> bool {
    should_parse(s) && !matches!(s, Some(CompileStage::Parse))
}

#[cfg(feature = "codegen")]
const fn should_codegen(s: &Option<CompileStage>) -> bool {
    if should_validate(s) {
        !matches!(s, Some(CompileStage::Validate))
    } else {
        false
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
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

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("Non-Ascii Character :(")]
    NoAscii(#[from] NotAscii),
    #[error("Lexing: {0}")]
    Lexing(lex::Error),

    #[cfg(feature = "parse")]
    #[error("Parsing: {0}")]
    Parsing(parse::Error),

    #[cfg(feature = "semantics")]
    #[error("Semantics: {0}")]
    Semantics(semantics::Error),
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
