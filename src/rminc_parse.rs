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
        if p.starts_with(|c: char| c.is_ascii_punctuation()) {
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
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tok_len: tokens.len(),
            tokens,
            cur: 0,
        }
    }

    fn consume(&mut self, op: &str) -> bool {
        if self.cur == self.tok_len {
            return false;
        }
        if let TokenKind::Reserved(str) = self.tokens[self.cur].kind {
            if str == op {
                self.cur += 1;
                return true;
            }
        }
        false
    }

    fn skip(&mut self, op: &str) {
        if !self.consume(op) {
            panic!("{} is expected", op)
        }
    }

    fn expect_number(&mut self) -> rminc_ast::Expr {
        if self.cur == self.tok_len {
            panic!("number is expected");
        }
        if let TokenKind::Num(num) = self.tokens[self.cur].kind {
            self.cur += 1;
            return rminc_ast::Expr::IntLiteral(num);
        }
        panic!("number is expected");
    }

    /// expr    = mul ("+" mul | "-" mul)*
    fn expr(&mut self) -> rminc_ast::Expr {
        let mut node = self.mul();
        loop {
            if self.consume("+") {
                node = rminc_ast::Expr::Op(String::from("+"), Vec::from([node, self.mul()]));
                continue;
            }
            if self.consume("-") {
                node = rminc_ast::Expr::Op(String::from("-"), Vec::from([node, self.mul()]));
                continue;
            }
            return node;
        }
    }

    /// mul     = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> rminc_ast::Expr {
        let mut node = self.unary();
        loop {
            if self.consume("*") {
                node = rminc_ast::Expr::Op(String::from("*"), Vec::from([node, self.unary()]));
                continue;
            }
            if self.consume("/") {
                node = rminc_ast::Expr::Op(String::from("/"), Vec::from([node, self.unary()]));
                continue;
            }
            return node;
        }
    }

    /// unary   = ("+" | "-")? primary
    fn unary(&mut self) -> rminc_ast::Expr {
        if self.consume("+") {
            return self.primary();
        }
        if self.consume("-") {
            return rminc_ast::Expr::Op(
                String::from("-"),
                Vec::from([rminc_ast::Expr::IntLiteral(0), self.primary()]),
            );
        }
        self.primary()
    }

    /// primary = num | "(" expr ")"
    fn primary(&mut self) -> rminc_ast::Expr {
        if self.consume("(") {
            let node = self.expr();
            self.skip(")");
            return node;
        }
        self.expect_number()
    }

    fn parse(&mut self) -> rminc_ast::Program {
        let def = rminc_ast::Def::Fun(
            String::from("main"),
            Vec::new(),
            rminc_ast::TypeExpr::Primitive(String::from("long")),
            rminc_ast::Stmt::Return(self.expr()),
        );
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
        println!("{:?}", str_to_ast("1*2+(3+4)"));
        println!("{:?}", str_to_ast("-3*+5+30"));
        assert_eq!(Some((120, "hogehgoe")), get_number("120hogehgoe"));
        assert_eq!(Some((120, "")), get_number("120"));
        assert_eq!(Some((0, "")), get_number("0"));
        assert_eq!(None, get_number("a0"));
        assert_eq!(None, get_number("a"));
        assert_eq!(None, get_number(""));
    }
}
