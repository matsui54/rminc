use crate::rminc_ast;

#[derive(Debug)]
enum TokenKind<'a> {
    Reserved(&'a str),
    Num(i64),
}

#[derive(Debug)]
struct Token<'a> {
    kind: TokenKind<'a>,
    at: usize,
}

struct Tokenizer<'a> {
    code: &'a str,
    cur: usize,
}

impl Tokenizer<'_> {
    fn new(code: &str) -> Tokenizer {
        Tokenizer { code, cur: 0 }
    }

    fn skip_space(&mut self) {
        match self.code[self.cur..].find(|c: char| !c.is_whitespace()) {
            Some(pos) => {
                self.cur += pos;
            }
            None => {
                self.cur = self.code.len() - 1;
            }
        }
    }

    fn get_number(&mut self) -> Option<i64> {
        let rem = &self.code[self.cur..];
        match rem.find(|c: char| !c.is_digit(10)) {
            Some(pos) => {
                if pos == 0 {
                    None
                } else {
                    self.cur += pos;
                    Some(rem[..pos].parse::<i64>().unwrap())
                }
            }
            None => {
                self.cur = self.code.len() - 1;
                Some(rem.parse::<i64>().unwrap())
            }
        }
    }

    fn get_punct_size(&self) -> usize {
        let rem = &self.code[self.cur..];
        if rem.starts_with("==")
            || rem.starts_with("!=")
            || rem.starts_with("<=")
            || rem.starts_with(">=")
        {
            2
        } else if rem.starts_with(|c: char| c.is_ascii_punctuation()) {
            1
        } else {
            0
        }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        while self.cur < self.code.len() - 1 {
            self.skip_space();

            if let Some(num) = self.get_number() {
                tokens.push(Token {
                    kind: TokenKind::Num(num),
                    at: self.cur,
                });
                continue;
            }
            let ps = self.get_punct_size();
            if ps != 0 {
                tokens.push(Token {
                    kind: TokenKind::Reserved(&self.code[self.cur..(self.cur + ps)]),
                    at: self.cur,
                });
                self.cur += ps;
            }
        }
        tokens
    }
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

    /// expr       = equality
    fn expr(&mut self) -> rminc_ast::Expr {
        self.equality()
    }

    /// equality   = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> rminc_ast::Expr {
        let mut node = self.relational();
        loop {
            if self.consume("==") {
                node =
                    rminc_ast::Expr::Op(String::from("=="), Vec::from([node, self.relational()]));
                continue;
            }
            if self.consume("!=") {
                node =
                    rminc_ast::Expr::Op(String::from("!="), Vec::from([node, self.relational()]));
                continue;
            }
            return node;
        }
    }

    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> rminc_ast::Expr {
        let mut node = self.add();
        loop {
            if self.consume("<") {
                node = rminc_ast::Expr::Op(String::from("<"), Vec::from([node, self.add()]));
                continue;
            }
            if self.consume("<=") {
                node = rminc_ast::Expr::Op(String::from("<="), Vec::from([node, self.add()]));
                continue;
            }
            if self.consume(">") {
                node = rminc_ast::Expr::Op(String::from(">"), Vec::from([node, self.add()]));
                continue;
            }
            if self.consume(">=") {
                node = rminc_ast::Expr::Op(String::from(">="), Vec::from([node, self.add()]));
                continue;
            }
            return node;
        }
    }

    /// add        = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> rminc_ast::Expr {
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

    /// unary = ("+" | "-") unary | primary
    fn unary(&mut self) -> rminc_ast::Expr {
        if self.consume("+") {
            return self.unary();
        }
        if self.consume("-") {
            return rminc_ast::Expr::Op(
                String::from("-"),
                Vec::from([rminc_ast::Expr::IntLiteral(0), self.unary()]),
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
    let mut tokenizer = Tokenizer::new(code);
    let tokens = tokenizer.tokenize();
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
