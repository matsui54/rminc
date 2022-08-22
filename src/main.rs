use std::fs::File;
use std::io::Write;

fn main() {
    let args : Vec<String> = std::env::args().collect();
    let file_xml = &args[1];
    let file_asm = &args[2];
    let mut file = File::create(file_asm).unwrap();
    file.write_all(String::from("hoge").as_bytes()).unwrap();
}
