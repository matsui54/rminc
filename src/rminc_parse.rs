use crate::rminc_ast;

#[derive(Debug)]
enum TokenKind<'a> {
    Reserved(&'a str),
    Num(i64),
    Eof,
}

#[derive(Debug)]
struct Token<'a> {
    kind: TokenKind<'a>,
    // str: &'a str,
}

fn get_number(code: &str) -> Option<(i64, &str)> {
    if code.len() == 0 {
        return None;
    }
    match code.find(|c: char| !c.is_digit(10)) {
        Some(pos) => {
            if pos == 0 {
                None
            } else {
                Some((code[0..pos].parse::<i64>().unwrap(), &code[pos..]))
            }
        }
        None => Some((code.parse::<i64>().unwrap(), "")),
    }
}

fn tokenize(code: &str) -> Vec<Token> {
    let mut p = code;
    let mut tokens: Vec<Token> = Vec::new();
    while p.len() != 0 {
        p = p.trim_left();

        match get_number(p) {
            Some((val, rem)) => {
                tokens.push(Token {
                    kind: TokenKind::Num(val),
                });
                p = rem;
            }
            None => {}
        }
        if p.starts_with(&['+', '-']) {
            tokens.push(Token {
                kind: TokenKind::Reserved(&p[0..1]),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let tokens = tokenize("1+1");
        println!("{:?}", tokens);
        assert_eq!(Some((120, "hogehgoe")), get_number("120hogehgoe"));
        assert_eq!(Some((120, "")), get_number("120"));
        assert_eq!(Some((0, "")), get_number("0"));
        assert_eq!(None, get_number("a0"));
        assert_eq!(None, get_number("a"));
        assert_eq!(None, get_number(""));
    }
}
