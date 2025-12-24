mod pseudo_pass;
mod tacky_opt;
mod tacky_pass;
mod x86_pass;

use ast::semantics::SymbolTable;
//pub use assembly::Binary;
//use assembly::Program;
use crate::Optimizations;
use asm::x86::Target;
use ast::semantics::typed;

pub fn generate(
    program: typed::Program,
    emit_asm: bool,
    opt: &Optimizations,
    target: Target,
    mut table: SymbolTable,
) -> Box<[u8]> {
    let tacky = tacky_pass::emit(program, &mut table);
    let tacky = tacky_opt::opt(tacky, opt, &table);
    //eprintln!("{tacky:?}");
    if emit_asm {
        let (pseudo, table) = pseudo_pass::emit(tacky, table);

        let asm = x86_pass::fix_ast(pseudo, &table, &target);
        asm::x86::emit(&asm, target)
    } else {
        Box::new([])
    }
}
