pub mod parse;
pub mod semantics;
mod stmnt_path;
mod token;
mod var_type;

pub type Ident = String;
pub type Token = token::Token<Ident>;
pub type DebugToken = token::DebugToken<Ident>;
pub type Arr<T> = Box<[T]>;

pub use parse::inc_dec::{IncDec, IncOp, POST_DEC, POST_INC, PRE_DEC, PRE_INC};
pub use stmnt_path::StatementPath;
pub use token::Constant;
pub use var_type::VarType;
