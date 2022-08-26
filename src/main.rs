use std::fs::File;
use std::io::Write;

mod rminc_ast;
mod rminc_cogen;

/// # reference
/// https://www.sigbus.info/compilerbook
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let argc = args.len();
    let file_asm = if argc > 2 { &args[2] } else { "ex.s" };

    let ast = rminc_ast::Program { defs: Vec::new() };

    let asm = rminc_cogen::ast_to_asm_program(ast);

    let mut file = File::create(file_asm).unwrap();
    file.write_all(asm.as_bytes()).unwrap();
}
