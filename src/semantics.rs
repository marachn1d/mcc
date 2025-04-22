pub mod ast;
mod check_labels;
mod resolve;
pub mod typecheck;
use crate::lex::Identifier;
use crate::parse::ParamList;
use crate::parse::Program as AstProgram;
use crate::parse::StorageClass;
pub use check_labels::check as check_labels;
pub use typecheck::Attr;
pub use typecheck::StaticInit;
pub use typecheck::SymbolTable;
mod resolve_loops;
pub use ast::label_prelude as labeled;
pub use ast::type_prelude as typed;
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
// move this into ast
#[derive(Debug, Copy, Clone)]
pub struct LabelId(usize);

#[derive(Debug, Clone)]
pub struct StatementLabels {
    pub start: Identifier,
    pub r#break: Identifier,
    pub r#continue: Identifier,
    pub end: Identifier,
}

use crate::lex::Constant;

impl LabelId {
    pub fn labels(&self) -> StatementLabels {
        StatementLabels {
            start: Identifier::from(format!("s{}s", self.0)),
            r#break: self.r#break(),
            r#continue: self.r#continue(),
            end: Identifier::from(format!("s{}e", self.0)),
        }
    }

    pub fn r#break(&self) -> Identifier {
        Identifier::from(format!("s{}b", self.0))
    }

    pub fn case(&self, value: Constant) -> Identifier {
        Identifier::from(format!("sc{}{}", self.0, value,))
    }
    pub fn default(&self) -> Identifier {
        Identifier::from(format!("sc{}d", self.0))
    }
    pub fn r#continue(&self) -> Identifier {
        Identifier::from(format!("s{}c", self.0))
    }
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
