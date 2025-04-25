mod labeled;
mod parse;
mod token;
mod typed;

pub use super::c_types::{Constant, FnType, StaticInit, VarType};
pub(crate) use super::RefKey as Key;
pub use parse::Program as ParseProgram;
pub use token::{DebugToken, Token};
pub mod incdec {
    pub use super::parse::inc_dec::{Fix, IncDec, IncOp, POST_DEC, POST_INC, PRE_DEC, PRE_INC};
}

pub mod parse_prelude {
    pub use super::parse::prelude::*;
    pub(crate) use super::{FnType, VarType};
    pub(crate) use super::{Key, Token};
}

pub mod labeled_prelude {
    pub use super::parse::prelude::*;
    pub use super::token::Token;
}

pub mod typed_prelude {
    pub use super::token::Token;
    pub use super::typed::prelude::*;
}

#[derive(Debug, Copy, Clone)]
pub struct LabelId(usize);

#[derive(Debug, Copy, Clone)]
pub struct Label {
    id: usize,
    pos: LabelPos,
}

#[derive(Debug, Copy, Clone)]
pub enum LabelPos {
    Start,
    Break,
    Continue,
    End,
    Case(Constant),
    Default,
}

impl LabelId {
    pub const fn start(&self) -> Label {
        self.pos(LabelPos::Start)
    }

    pub const fn r#break(&self) -> Label {
        self.pos(LabelPos::Break)
    }

    pub const fn r#continue(&self) -> Label {
        self.pos(LabelPos::Continue)
    }

    pub const fn end(&self) -> Label {
        self.pos(LabelPos::End)
    }

    pub const fn case(&self, c: Constant) -> Label {
        self.pos(LabelPos::Case(c))
    }

    pub const fn default(&self) -> Label {
        self.pos(LabelPos::Case(c))
    }

    const fn pos(&self, pos: LabelPos) -> Label {
        Label { id: self.0, pos }
    }
}

use std::fmt::{self, Display, Formatter};
impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LabelPos::{Break, Case, Continue, Default, End, Start};
        let id = self.id;
        match self.pos {
            Break => write!(f, "s{id}b"),
            Continue => write!(f, "s{id}c"),
            Case(c) => write!(f, "sc{id}{}", c),
            Default => write!(f, "s{id}d"),
            Start => write!(f, "s{id}tart"),
            End => write!(f, "e{id}nd"),
        }
    }
}
