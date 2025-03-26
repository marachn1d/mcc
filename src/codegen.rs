mod assembly;
mod pseudo_pass;
mod tacky_pass;
mod x86_pass;

use super::lex::Identifier;
use crate::semantics::typed;
use crate::semantics::SymbolTable;
pub use assembly::Binary;
use assembly::Program;

pub fn generate(program: typed::Program, emit_asm: bool, mut table: SymbolTable) -> Box<[u8]> {
    let tacky = tacky_pass::emit(program, &mut table);
    if emit_asm {
        let (pseudo, table) = pseudo_pass::emit(tacky, table);
        let asm = x86_pass::fix_ast(pseudo, &table);
        assembly::emit(&asm)
    } else {
        Box::new([])
    }
}
