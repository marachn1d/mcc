pub mod c_vals;
pub mod expr;
pub mod labeled;
pub mod parse;
pub mod token;
pub mod typed;
pub use c_vals::Constant;
pub use expr::*;
pub use symtab::Key;
pub use token::{DebugToken, Token};

pub(crate) mod prelude {
    pub use super::c_vals::*;
    pub use super::expr::*;
    use super::labeled;
    pub use super::Bop;
    pub use super::StorageClass;
    pub use super::UnOp;
    pub use symtab::Key;
}

pub mod types_prelude {
    pub use super::c_vals::{FnType, VarType};
}

#[derive(Debug, Copy, Clone)]
pub enum StorageClass {
    Static,
    Extern,
}

impl PartialEq for StorageClass {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Static, Self::Static) | (Self::Extern, Self::Extern)
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LabelId(pub usize);

impl std::fmt::Display for LabelId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Label {
    pub id: usize,
    pub pos: LabelPos,
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

use std::sync::atomic::{AtomicUsize, Ordering};
static LOOP_COUNTER: AtomicUsize = AtomicUsize::new(0);

impl LabelId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(LOOP_COUNTER.fetch_add(1, Ordering::AcqRel))
    }

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
        self.pos(LabelPos::Default)
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
