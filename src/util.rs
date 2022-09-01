use crate::tokenize::Token;
use std::process;

pub fn error_at(code: &str, pos: usize, message: &str) -> ! {
    eprint!("{code}");
    eprintln!("{}^ {}", " ".repeat(pos), message);
    process::exit(1)
}

pub fn error_tok(code: &str, tok: &Token, message: &str) -> ! {
    eprintln!("Error while reading token {:?}", tok);
    error_at(code, tok.at, message)
}
