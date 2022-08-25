use std::fs::File;
use std::io::Write;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let argc = args.len();
    let file_asm = if argc > 2 { &args[2] } else { "ex.s" };
    let mut file = File::create(file_asm).unwrap();
    file.write_all(String::from("hoge\n").as_bytes()).unwrap();
}
