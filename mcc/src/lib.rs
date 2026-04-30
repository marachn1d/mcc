//pub mod ast;
use clap::{Args};
use clap::builder::ArgPredicate;

use std::fs;
use std::io;
use std::path::PathBuf;

// TODO: Define token type, let it be slow rn

pub mod lex;
pub use ast::DebugToken;
pub use ast::Token;
use asm::x86::Target;

pub use util::TokenIter;

#[cfg(feature = "codegen")]
pub mod codegen;
#[cfg(feature = "parse")]
pub mod parse;
#[cfg(feature = "semantics")]
pub mod semantics;

use std::sync::OnceLock;
pub static CONFIG: OnceLock<Config> = OnceLock::new();
pub struct Config {
    pub stage: CompileStage,
    pub opt: Optimizations,
    pub version: CVersion,
    pub target:Target,
    pub emit_asm:bool,
}

#[derive(Default, Copy, Clone, clap::Args)]
pub struct Optimizations {
    #[arg(long="fold-constants", default_value_if("optimize",ArgPredicate::IsPresent,"true"))]
    pub constant_folding: bool,
    #[arg(long="propogate-copies",default_value_if("optimize",ArgPredicate::IsPresent,"true"))]
    pub copy_propogation: bool,
    #[arg(long="eliminate-unreachable-code",default_value_if("optimize",ArgPredicate::IsPresent,"true"))]
    pub unreachable_code: bool,
    #[arg(long="eliminate-dead_stores", default_value_if("optimize",ArgPredicate::IsPresent,"true"))]
    pub dead_store: bool,
    #[arg(long = "optimize")]
    pub all: bool,
}

impl Optimizations {
    pub const fn all() -> Self {
        Self {
            constant_folding: true,
            copy_propogation: true,
            unreachable_code: true,
            dead_store: true,
            all:true,
        }
    }

    pub const fn all_disabled(&self) -> bool {
        (self.constant_folding | self.copy_propogation | self.unreachable_code | self.dead_store)
            == false
    }
}

pub fn compile(path: PathBuf) -> Result<Option<PathBuf>, Error> {
    let bytes = fs::read(&path).map_err(|_| Error::InvalidInput)?;
    let _ = fs::remove_file(&path);
    let stage = CONFIG.get().unwrap().stage;
    try_compile(&bytes, &stage, path)
}

fn try_compile(bytes:&[u8], stage:&CompileStage, path:PathBuf) -> Result<Option<PathBuf>, Error>{
    if stage.should_lex() {
        let tokens = lex::tokenize(&bytes)?;
        if stage.should_parse() && cfg!(feature="parse"){
        try_parse(tokens, stage, path)
        }else{
            Ok(None)
        }
    }else{
        Ok(None)
    }
}

#[cfg(feature="parse")]
fn try_parse(tokens:Box<[DebugToken]>, stage:&CompileStage, path:PathBuf) -> Result<Option<PathBuf>,Error>{ 

        let ast = parse(tokens)?;
        if stage.should_validate() && cfg!(feature = "semantics"){
            try_semantics(ast, stage, path)
        }else{
            Ok(None)
        }
}

#[cfg(feature = "semantics")]
fn try_semantics(ast:ast::parse::Program, stage:&CompileStage, path:PathBuf) -> Result<Option<PathBuf>, Error>{

    let (program, map) = semantics::check(ast)?;
    if stage.should_codegen() && cfg!(feature = "codegen"){
        try_codegen(program, map, path)
    }else{
        Ok(None)
    }
}

#[cfg(feature = "codegen")]
fn try_codegen(program: ast::semantics::typed::Program, map: ast::semantics::SymbolTable, mut path: PathBuf) -> Result<Option<PathBuf>,Error>{

    let (opt, target, keep_asm) = {
        let conf = CONFIG.get().unwrap();
        (conf.opt, conf.target, conf.emit_asm)
    };
    let code = codegen::generate(
                        program,
                        true,
                        &opt,
                        target,
                        map,
    );
        path.set_extension("s");
    fs::write(&path, &code)?;
    Ok(Some(path))
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

#[derive(PartialEq, Eq, Copy, Clone, Debug, clap::ValueEnum)]
pub enum CompileStage {
    Lex,
    Parse,
    Codegen,
    Tacky,
    Validate,
}

impl CompileStage{
    const fn should_lex(&self) -> bool{
        self.should_parse() || true
    }

    const fn should_parse(&self) -> bool{
        self.should_validate() || matches!(self, Self::Parse)
    }

    const fn should_validate(&self) -> bool{
        self.should_tacky() || matches!(self, Self::Validate)
    }

    const fn should_tacky(&self) -> bool{
        self.should_codegen() || matches!(self, Self::Tacky)
    }

    const fn should_codegen(&self) -> bool{
        matches!(self, Self::Codegen)
    }
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
