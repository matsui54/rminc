use crate::rminc_ast;

#[derive(Debug)]
enum TokenKind<'a> {
    Reserved(&'a str),
    Num(i64),
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
        p = p.trim_start();

        if let Some((val, rem)) = get_number(p) {
            tokens.push(Token {
                kind: TokenKind::Num(val),
            });
            p = rem;
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

#[derive(Debug)]
struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    cur: usize,
    tok_len: usize,
}

impl Parser<'_> {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tok_len: tokens.len(),
            tokens,
            cur: 0,
        }
    }

    fn consume(&mut self, op: &str) -> bool {
        if let TokenKind::Reserved(str) = self.tokens[self.cur].kind {
            if str == op {
                self.cur += 1;
                return true;
            }
        }
        false
    }

    fn expect(&mut self, op: &str) {
        if let TokenKind::Reserved(str) = self.tokens[self.cur].kind {
            if str == op {
                self.cur += 1;
                return;
            }
        }
        panic!("{} is expected", op)
    }

    fn expect_number(&mut self) -> i64 {
        if let TokenKind::Num(num) = self.tokens[self.cur].kind {
            self.cur += 1;
            return num;
        }
        panic!("number is expected");
    }

    pub fn parse(&mut self) -> rminc_ast::Program {
        let def = rminc_ast::Def::Fun(
            String::from("main"),
            Vec::new(),
            rminc_ast::TypeExpr::Primitive(String::from("long")),
            rminc_ast::Stmt::Empty,
        );
        println!("{}", self.expect_number());
        while self.cur < self.tok_len {
            if self.consume("+") {
                println!("+\n{}", self.expect_number());
                continue;
            }
            self.expect("-");
            println!("-\n{}", self.expect_number());
        }
        rminc_ast::Program {
            defs: Vec::from([def]),
        }
    }
}

pub fn str_to_ast(code: &str) -> rminc_ast::Program {
    let tokens = tokenize(code);
    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        println!("{:?}", tokenize("1+1"));
        println!("{:?}", tokenize("  1 +       1+213213- 22 "));
        println!("{:?}", str_to_ast("  1 +       1+213213- 22 "));
        assert_eq!(Some((120, "hogehgoe")), get_number("120hogehgoe"));
        assert_eq!(Some((120, "")), get_number("120"));
        assert_eq!(Some((0, "")), get_number("0"));
        assert_eq!(None, get_number("a0"));
        assert_eq!(None, get_number("a"));
        assert_eq!(None, get_number(""));
    }
}
