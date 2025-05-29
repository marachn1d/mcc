mod check_labels;
mod resolve;
pub mod typecheck;
pub use ast::attr::{Attr, SymbolTable};
pub use ast::c_vals::StaticInit;
use ast::parse::Program as AstProgram;
pub use check_labels::check as check_labels;
use symtab::Store;
mod resolve_loops;
pub use resolve::resolve;

pub fn check<'a>(
    mut program: AstProgram<'a>,
    store: &'a Store,
) -> Result<(ast::typed::Program<'a>, SymbolTable<'a>), Error> {
    resolve(&mut program, store).map_err(Error::Resolve)?;

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

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Label(LabelError),

    #[error(transparent)]
    Loops(LoopError),

    #[error(transparent)]
    Resolve(ResolveError),

    #[error(transparent)]
    TypeCheck(typecheck::Error),
}

pub use check_labels::Error as LabelError;
pub use resolve::Error as ResolveError;
use resolve_loops::Error as LoopError;
