mod assembly;
mod pseudo_pass;
use std::collections::HashMap;
mod tacky_pass;
//mod x86_pass;

use ast::attr::{self, Attr};
pub type SymbolTable<'a> = HashMap<SymKey<'a>, Attr>;
/*
pub use assembly::Binary;
use assembly::Program;
*/

pub fn generate(
    program: &ast::typed::Program,
    emit_asm: bool,
    mut table: attr::SymbolTable,
) -> Box<[u8]> {
    let mut symtab: HashMap<SymKey, Attr> = table
        .drain()
        .map(|(key, val)| (SymKey::from(key), val))
        .collect();

    let tacky = tacky_pass::emit(&program, &mut symtab);

    if emit_asm {
        let (pseudo, table) = pseudo_pass::emit(tacky, symtab);
        /*
        let asm = x86_pass::fix_ast(pseudo, &table);
        assembly::emit(&asm)
        */
        todo!()
    } else {
        Box::new([])
    }
}

#[derive(PartialEq, Eq, Hash)]
enum SymKey<'a> {
    Key(ast::Key<'a>),
    Var(asm::tacky::TackyTmp),
}

impl<'a> From<ast::Key<'a>> for SymKey<'a> {
    fn from(value: ast::Key<'a>) -> Self {
        Self::Key(value)
    }
}
