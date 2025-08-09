pub mod parse;
mod token;
mod var_type;

pub type Ident = String;
pub type Token = token::Token<Ident>;
pub type DebugToken = token::DebugToken<Ident>;

pub use token::Constant;
pub use var_type::VarType;
