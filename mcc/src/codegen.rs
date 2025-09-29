mod pseudo_pass;
mod tacky_opt;
mod tacky_pass;
mod x86_pass;

use ast::semantics::SymbolTable;
//pub use assembly::Binary;
//use assembly::Program;
use crate::Optimizations;
use ast::semantics::typed;
use asm::x86::Target;

pub fn generate(
    program: typed::Program,
    emit_asm: bool,
    opt: &Optimizations,
    target: Target,
    mut table: SymbolTable,
) -> Box<[u8]> {
    let tacky = {
        let mut tacky = tacky_pass::emit(program, &mut table);
        tacky_opt::opt(&mut tacky, opt, &table);
        tacky
    };

    if emit_asm {
        let (pseudo, table) = pseudo_pass::emit(tacky, table);

        let asm = x86_pass::fix_ast(pseudo, &table);
        asm::x86::emit(&asm, target)
    } else {
        Box::new([])
    }
}
