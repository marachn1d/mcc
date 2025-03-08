mod assembly;
mod pseudo_pass;
mod tacky_pass;
mod x86_pass;

use super::lex::Identifier;
use super::semantics::Program as AstProgram;

use crate::semantics::SymbolTable;
pub use assembly::Binary;
use assembly::Program;

pub fn generate(program: AstProgram, emit_asm: bool, table: &SymbolTable) -> Box<[u8]> {
    let tacky = tacky_pass::emit(program, table);

    if emit_asm {
        let pseudo = pseudo_pass::emit(tacky);
        let asm = x86_pass::fix_ast(pseudo, table);
        assembly::emit(&asm)
    } else {
        Box::new([])
    }
}
