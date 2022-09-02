use std::fs::File;
use std::io::Write;

use parse::Parser;
use tokenize::Tokenizer;

mod ast;
mod codegen;
mod parse;
mod tokenize;
mod util;

fn gen_asm(code: &str) -> String {
    let mut tokenizer = Tokenizer::new(&code);
    let tokens = tokenizer.tokenize();
    println!("{:?}", tokens);

    let ast = Parser::new(tokens, code).parse();
    println!("{:?}", ast);

    codegen::ast_to_asm_program(ast)
}

/// # reference
/// https://www.sigbus.info/compilerbook
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let argc = args.len();
    let file_code = if argc > 1 { &args[1] } else { "ex.c" };
    let file_asm = if argc > 2 { &args[2] } else { "ex.s" };

    let code = std::fs::read_to_string(file_code).expect(&format!("Failed to open {}", file_code));

    let asm = gen_asm(&code);

    let mut file = File::create(file_asm).unwrap();
    file.write_all(asm.as_bytes()).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand;
    use std::process::{Command, Stdio};

    fn check_output(code: &str, ref_v: i32) {
        let r = rand::random::<u64>();
        let exe_fname = format!("./tmp{}", r);
        let asm_fname = format!("./tmp{}.s", r);

        let asm = gen_asm(code);
        let mut file = File::create(&asm_fname).unwrap();
        file.write_all(asm.as_bytes()).unwrap();
        {
            let output = Command::new("gcc")
                .arg("-static")
                .arg("-o")
                .arg(&exe_fname)
                .arg(&asm_fname)
                .stderr(Stdio::inherit())
                .output()
                .unwrap_or_else(|e| panic!("failed to execute process: {}", e));
            assert!(output.status.success());
        }

        {
            let output = Command::new(exe_fname)
                .output()
                .unwrap_or_else(|e| panic!("failed to execute process: {}", e));
            assert_eq!(output.status.code().unwrap(), ref_v);
        }
    }

    #[test]
    fn arithmetic() {
        check_output("{return 1;}\n", 1);
        check_output("{return 42;}\n", 42);
        check_output("{return 5+20-4;}\n", 21);
        check_output("{return  12 + 34 - 5 ;}\n", 41);
        check_output("{return 5+6*7;}\n", 47);
        check_output("{return 5*(9-6);}\n", 15);
        check_output("{return (3+5)/2;}\n", 4);
        check_output("{return -10+20;}\n", 10);
        check_output("{return - -10;}\n", 10);
        check_output("{return - - +10;}\n", 10);
    }
    #[test]
    fn cmp() {
        check_output("{return 0==1;}\n", 0);
        check_output("{return 42==42;}\n", 1);
        check_output("{return 0!=1;}\n", 1);
        check_output("{return 42!=42;}\n", 0);

        check_output("{return 0<1;}\n", 1);
        check_output("{return 1<1;}\n", 0);
        check_output("{return 2<1;}\n", 0);
        check_output("{return 0<=1;}\n", 1);
        check_output("{return 1<=1;}\n", 1);
        check_output("{return 2<=1;}\n", 0);

        check_output("{return 1>0;}\n", 1);
        check_output("{return 1>1;}\n", 0);
        check_output("{return 1>2;}\n", 0);
        check_output("{return 1>=0;}\n", 1);
        check_output("{return 1>=1;}\n", 1);
        check_output("{return 1>=2;}\n", 0);
    }

    #[test]
    fn variable() {
        check_output("{long a; a=3; return a;}\n", 3);
        check_output("{long a; long z; a=3; z=5; return a+z;}\n", 8);

        check_output("{long a; long b; a=b=3; return a+b;}\n", 6);
        check_output("{long foo; foo=3; return foo;}\n", 3);
        check_output(
            "{long foo123; long bar; foo123=3; bar=5; return foo123+bar;}\n",
            8,
        );
    }

    #[test]
    fn others() {
        check_output("{return 1; 2; 3;}\n", 1);
        check_output("{1; return 2; 3;}\n", 2);
        check_output("{1; 2; return 3;}\n", 3);

        check_output("{ {1; {2;} return 3;} }\n", 3);
        check_output("{ ;;; return 5; }\n", 5);
    }

    #[test]
    fn test_if() {
        check_output("{ if (0) return 2; return 3; }\n", 3);
        check_output("{ if (1-1) return 2; return 3; }\n", 3);
        check_output("{ if (1) return 2; return 3; }\n", 2);
        check_output("{ if (2-1) return 2; return 3; }\n", 2);
        check_output("{ if (0) { 1; 2; return 3; } else { return 4; } }\n", 4);
        check_output("{ if (1) { 1; 2; return 3; } else { return 4; } }\n", 3);
    }

    #[test]
    fn test_while_for() {
        check_output(
            "{ long i; long j;i=0;j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }\n",
            55,
        );
        check_output("{ for (;;) {return 3;} return 5; }\n", 3);

        check_output("{ long i; i=0; while(i<10) { i=i+1; } return i; }\n", 10);
    }
}
