use crate::ast;
use crate::tokenize::{Token, TokenKind};

#[derive(Debug)]
pub struct Parser<'a> {
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

    fn expect_number(&mut self) -> ast::Expr {
        if self.cur == self.tok_len {
            panic!("number is expected");
        }
        if let TokenKind::Num(num) = self.tokens[self.cur].kind {
            self.cur += 1;
            return ast::Expr::IntLiteral(num);
        }
        panic!("number is expected");
    }

    /// expr       = equality
    fn expr(&mut self) -> ast::Expr {
        self.equality()
    }

    /// equality   = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> ast::Expr {
        let mut node = self.relational();
        loop {
            if self.consume("==") {
                node = ast::Expr::Op(String::from("=="), Vec::from([node, self.relational()]));
                continue;
            }
            if self.consume("!=") {
                node = ast::Expr::Op(String::from("!="), Vec::from([node, self.relational()]));
                continue;
            }
            return node;
        }
    }

    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> ast::Expr {
        let mut node = self.add();
        loop {
            if self.consume("<") {
                node = ast::Expr::Op(String::from("<"), Vec::from([node, self.add()]));
                continue;
            }
            if self.consume("<=") {
                node = ast::Expr::Op(String::from("<="), Vec::from([node, self.add()]));
                continue;
            }
            if self.consume(">") {
                node = ast::Expr::Op(String::from(">"), Vec::from([node, self.add()]));
                continue;
            }
            if self.consume(">=") {
                node = ast::Expr::Op(String::from(">="), Vec::from([node, self.add()]));
                continue;
            }
            return node;
        }
    }

    /// add        = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> ast::Expr {
        let mut node = self.mul();
        loop {
            if self.consume("+") {
                node = ast::Expr::Op(String::from("+"), Vec::from([node, self.mul()]));
                continue;
            }
            if self.consume("-") {
                node = ast::Expr::Op(String::from("-"), Vec::from([node, self.mul()]));
                continue;
            }
            return node;
        }
    }

    /// mul     = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> ast::Expr {
        let mut node = self.unary();
        loop {
            if self.consume("*") {
                node = ast::Expr::Op(String::from("*"), Vec::from([node, self.unary()]));
                continue;
            }
            if self.consume("/") {
                node = ast::Expr::Op(String::from("/"), Vec::from([node, self.unary()]));
                continue;
            }
            return node;
        }
    }

    /// unary = ("+" | "-") unary | primary
    fn unary(&mut self) -> ast::Expr {
        if self.consume("+") {
            return self.unary();
        }
        if self.consume("-") {
            return ast::Expr::Op(
                String::from("-"),
                Vec::from([ast::Expr::IntLiteral(0), self.unary()]),
            );
        }
        self.primary()
    }

    /// primary = num | "(" expr ")"
    fn primary(&mut self) -> ast::Expr {
        if self.consume("(") {
            let node = self.expr();
            self.skip(")");
            return node;
        }
        self.expect_number()
    }

    pub fn parse(&mut self) -> ast::Program {
        let def = ast::Def::Fun(
            String::from("main"),
            Vec::new(),
            ast::TypeExpr::Primitive(String::from("long")),
            ast::Stmt::Return(self.expr()),
        );
        ast::Program {
            defs: Vec::from([def]),
        }
    }
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
