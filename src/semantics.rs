mod check_labels;
mod resolve;
pub mod typecheck;
use crate::parse::ParamList;
use crate::parse::Program as AstProgram;
use crate::parse::StorageClass;
pub use check_labels::check as check_labels;
pub use typecheck::Attr;
pub use typecheck::StaticInit;
pub use typecheck::SymbolTable;
mod resolve_loops;
pub use crate::types::{ast::typed_prelude as typed, labeled_prelude as labeled};
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
