/*
 *  TODO:
 *
 *  map_enum crate? like metaprogramming to create a new type based on the enum
 *
 *  omg okay so i think my ideal version would be like a union of ControlFlow and logic, like I
 *  wish I could concisely decide to pass all the arguments or do things with the arguments right
 *  there
 *
 */

use super::StaticInit;
pub use crate::lex::Identifier;
use crate::parse;
pub use parse::inc_dec::*;
pub use parse::{Arr, ParamList};

use super::LabelId;
use crate::lex::Constant;
#[derive(Clone, Debug)]
pub enum Label {
    Named(Identifier),
    Default(LabelId),
    Case { c: Constant, id: LabelId },
}

pub mod label_prelude {
    pub use super::labeled::{Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec};
    pub use super::{Arr, Fix, Identifier, IncDec, Label, ParamList};
}

pub mod type_prelude {
    pub use super::typechecked::{
        Block, BlockItem, Dec, Expr, FnDec, ForInit, Program, Stmnt, VarDec,
    };
    pub use super::{Arr, Fix, Identifier, IncDec, Label, ParamList};
}
