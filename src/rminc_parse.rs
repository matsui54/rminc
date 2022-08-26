use crate::rminc_ast;

#[derive(Debug)]
enum TokenKind {
    Reserved,
    Num,
    Eof,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
}

fn tokenize(code: &str) -> Vec<Token> {
    let mut p = code;
    while p.len() != 0 {
        p = match p.strip_prefix("+") {
            Some(m) => m,
            None => p,
        };
    }
    Vec::from([Token {
        kind: TokenKind::Reserved,
    }])
}

pub fn str_to_ast(code: &String) -> rminc_ast::Program {
    let def = rminc_ast::Def::Fun(
        String::from("main"),
        Vec::new(),
        rminc_ast::TypeExpr::Primitive(String::from("long")),
        rminc_ast::Stmt::Empty,
    );
    let tokens = tokenize(code);
    rminc_ast::Program {
        defs: Vec::from([def]),
    }
}
