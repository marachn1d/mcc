pub mod tacky;
pub mod x86;

use std::collections::HashMap;
pub type SymbolTable<'a> = HashMap<ast::Key<'a>, AsmSym>;

pub enum AsmTy {
    LWord,
    QWord,
}

pub enum AsmSym {
    Obj {
        ty: AsmTy,
        is_static: bool,
    },
    #[allow(dead_code)]
    Fn {
        defined: bool,
    },
}
