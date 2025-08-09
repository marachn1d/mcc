//pub mod ast;

use std::fs;
use std::io;
use std::path::PathBuf;

// TODO: Define token type, let it be slow rn

pub mod lex;
pub use ast::DebugToken;
pub use ast::Token;

pub use util::TokenIter;

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
    {
        if !should_parse(&stage) {
            return Ok("".into());
        }
        let ast = parse(tokens)?;
        #[cfg(feature = "semantics")]
        {
            if !should_validate(&stage) {
                return Ok("".into());
            };
            let (program, map) = semantics::check(ast)?;

            #[cfg(feature = "codegen")]
            {
                if !should_codegen(&stage) {
                    return Ok("".into());
                } else {
                    let code = codegen::generate(program, should_emit(&stage), map);
                    path.set_extension("S");
                    fs::write(&path, &code)?;
                }
            }
        }

        Ok(path)
    }

    #[cfg(not(feature = "parse"))]
    Ok("".into())
}

const fn should_emit(s: &Option<CompileStage>) -> bool {
    should_codegen(s)
}
const fn should_parse(s: &Option<CompileStage>) -> bool {
    !matches!(s, Some(CompileStage::Lex))
}

const fn should_validate(s: &Option<CompileStage>) -> bool {
    should_parse(s) && !matches!(s, Some(CompileStage::Parse))
}

const fn should_codegen(s: &Option<CompileStage>) -> bool {
    if should_validate(s) {
        !matches!(s, Some(CompileStage::Validate))
    } else {
        false
    }
}

fn parse(tokens: Box<[DebugToken]>) -> Result<ast::parse::Program, Error> {
    let tokens = tokens.into_iter().map(|x| x.token).collect();
    Ok(parse::parse(tokens)?)
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
