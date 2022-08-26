use crate::rminc_ast;

#[derive(Debug)]
enum TokenKind {
    Reserved,
    Num(i64),
    Eof,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    str: &str,
}

fn tokenize(code: &str) -> Vec<Token> {
    let mut p = code;
    let mut tokens: Vec<Token> = Vec::new();
    while p.len() != 0 {
        p = p.trim_left();

        if p.starts_with(&['+', '-']) {
            tokens.push(Token {
                kind: TokenKind::Reserved,
                str: &p[0],
            });
            p = &p[1..];
            continue;
        }
    }
    tokens
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
