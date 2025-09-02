mod check_labels;
mod resolve;
pub mod typecheck;
use ast::semantics::{labeled, typed, Attr, SymbolTable};

use ast::parse::Program as AstProgram;
pub use ast::parse::StaticInit;
pub use check_labels::check as check_labels;
mod resolve_loops;
pub use resolve::resolve;

pub fn check(mut program: AstProgram) -> Result<(typed::Program, SymbolTable), Error> {
    resolve(&mut program).map_err(Error::Resolve)?;

    let labeled = resolve_loops::label(program).map_err(Error::Loops)?;

    let (symbol_table, program) = typecheck::typecheck(labeled).map_err(Error::TypeCheck)?;

    check_labels::check(&program, &symbol_table).map_err(Error::Label)?;

    Ok((program, symbol_table))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Int,
    Long,
}

#[derive(Debug, Copy, Clone)]
pub struct LabelId(usize);

use ast::Constant;

#[derive(Debug)]
pub enum Error {
    Label(LabelError),
    Loops(LoopError),
    Resolve(ResolveError),
    TypeCheck(typecheck::Error),
}

pub use check_labels::Error as LabelError;
pub use resolve::Error as ResolveError;
use resolve_loops::Error as LoopError;
