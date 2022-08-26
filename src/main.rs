use std::fs::File;
use std::io::Write;

mod rminc_ast;
mod rminc_cogen;
mod rminc_parse;

/// # reference
/// https://www.sigbus.info/compilerbook
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let argc = args.len();
    let file_code = if argc > 1 { &args[1] } else { "ex.c" };
    let file_asm = if argc > 2 { &args[2] } else { "ex.s" };

    let code = std::fs::read_to_string(file_code).expect(&format!("Failed to open {}", file_code));
    let ast = rminc_parse::str_to_ast(code);

    let asm = rminc_cogen::ast_to_asm_program(ast);

    let mut file = File::create(file_asm).unwrap();
    file.write_all(asm.as_bytes()).unwrap();
}
