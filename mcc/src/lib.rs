pub mod args;
pub mod config;

//pub mod ast;

use std::fs;
use std::io;
use std::path::PathBuf;

// TODO: Define token type, let it be slow rn

pub mod lex;
use asm::x86::Target;
pub use ast::DebugToken;
pub use ast::Token;

pub use util::TokenIter;

#[cfg(feature = "codegen")]
pub mod codegen;
#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "semantics")]
pub mod semantics;

use clap::Args;
use clap::ValueEnum;

use std::sync::OnceLock;
pub static CONFIG: OnceLock<Config> = OnceLock::new();
pub struct Config {
    pub stage: Option<CompileStage>,
    pub opt: Optimizations,
    pub version: CVersion,
    pub target: Target,
}

use clap::builder::ArgPredicate::IsPresent;
#[derive(Default, Copy, Clone, Args)]
pub struct Optimizations {
    #[arg(
        long = "fold-constants",
        default_value_if("optimize", IsPresent, "true")
    )]
    pub constant_folding: bool,

    #[arg(
        long = "propogate-copies",
        default_value_if("optimize", IsPresent, "true")
    )]
    pub copy_propogation: bool,

    #[arg(
        long = "eliminate-unreachable-code",
        default_value_if("optimize", IsPresent, "true")
    )]
    pub unreachable_code: bool,

    #[arg(
        long = "eliminate-dead-stores",
        default_value_if("optimize", IsPresent, "true")
    )]
    pub dead_store: bool,

    #[arg(long = "optimize", alias = "optimise")]
    optimize: bool,
}

impl Optimizations {
    pub const fn all() -> Self {
        Self {
            constant_folding: true,
            copy_propogation: true,
            unreachable_code: true,
            dead_store: true,
            optimize: true,
        }
    }

    pub const fn all_disabled(&self) -> bool {
        !(self.constant_folding | self.copy_propogation | self.unreachable_code | self.dead_store)
    }
}

pub fn compile(mut path: PathBuf) -> Result<PathBuf, Error> {
    let bytes = fs::read(&path).map_err(|_| Error::InvalidInput)?;
    let _ = fs::remove_file(&path);
    let (stage, opt, target) = {
        let conf = CONFIG.get().unwrap();
        (conf.stage, conf.opt, conf.target)
    };

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
                    let code = codegen::generate(program, should_emit(&stage), &opt, target, map);
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

#[derive(PartialEq, Eq, Copy, Clone, Debug, ValueEnum)]
pub enum CompileStage {
    Lex,

    Parse,
    Codegen,

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
