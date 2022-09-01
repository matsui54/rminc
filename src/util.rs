use crate::tokenize::Token;
use std::process;

// static void verror_at(char *loc, char *fmt, va_list ap) {
//   int pos = loc - current_input;
//   fprintf(stderr, "%s\n", current_input);
//   fprintf(stderr, "%*s", pos, ""); // print pos spaces.
//   fprintf(stderr, "^ ");
//   vfprintf(stderr, fmt, ap);
//   fprintf(stderr, "\n");
//   exit(1);
// }
pub fn error_at(code: &str, pos: usize, message: &str) -> ! {
    eprint!("{code}");
    eprintln!("{}^ {}", " ".repeat(pos - 1), message);
    process::exit(1)
}

pub fn error_tok(code: &str, tok: &Token, message: &str) -> ! {
    eprintln!("Error while reading token {:?}", tok);
    error_at(code, tok.at, message)
}
