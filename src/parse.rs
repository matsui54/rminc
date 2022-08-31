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

    fn equal(&mut self, op: &str) -> bool {
        if self.cur == self.tok_len {
            return false;
        }
        if let TokenKind::Reserved(str) = self.tokens[self.cur].kind {
            if str == op {
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

    /// program    = stmt*
    fn program(&mut self) -> ast::Program {
        let mut stmts = Vec::new();
        while self.cur < self.tok_len {
            stmts.push(self.stmt());
        }
        ast::Program {
            defs: Vec::from([ast::Def::Fun(
                String::from("main"),
                Vec::new(),
                ast::TypeExpr::Primitive(String::from("long")),
                ast::Stmt::Compound(Vec::new(), stmts),
            )]),
        }
    }

    /// stmt    = "{" compound_stmt
    ///         | "if" "(" expr ")" stmt ("else" stmt)?
    ///         | "while" "(" expr ")" stmt
    ///         | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    ///         | "return" expr ";"
    ///         | expr-stmt
    fn stmt(&mut self) -> ast::Stmt {
        let node: ast::Stmt;
        if let TokenKind::Keyword(kwd) = self.tokens[self.cur].kind {
            self.cur += 1;
            node = match kwd {
                "if" => {
                    self.skip("(");
                    let expr = self.expr();
                    self.skip(")");
                    let stmt = Box::new(self.stmt());
                    let stmt_else = if let TokenKind::Keyword(kwd) = self.tokens[self.cur].kind {
                        if kwd == "else" {
                            self.cur += 1;
                            Some(Box::new(self.stmt()))
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    ast::Stmt::If(expr, stmt, stmt_else)
                }
                "while" => {
                    self.skip("(");
                    let expr = self.expr();
                    self.skip(")");
                    let stmt = Box::new(self.stmt());
                    ast::Stmt::While(expr, stmt)
                }
                "for" => {
                    self.skip("(");
                    let expr0 = if !self.equal(";") {
                        Some(self.expr())
                    } else {
                        None
                    };
                    self.skip(";");
                    let expr1 = if !self.equal(";") {
                        Some(self.expr())
                    } else {
                        None
                    };
                    self.skip(";");
                    let expr2 = if !self.equal(")") {
                        Some(self.expr())
                    } else {
                        None
                    };
                    self.skip(")");
                    ast::Stmt::For(expr0, expr1, expr2, Box::new(self.stmt()))
                }
                "return" => {
                    let stmt = ast::Stmt::Return(self.expr());
                    self.skip(";");
                    stmt
                }
                _ => unreachable!("{} is unexpected", kwd),
            };
        } else if self.consume("{") {
            node = self.compound_stmt();
        } else {
            node = self.expr_stmt();
        }
        return node;
    }

    /// expr-stmt = expr? ";"
    fn expr_stmt(&mut self) -> ast::Stmt {
        if self.consume(";") {
            ast::Stmt::Empty
        } else {
            ast::Stmt::Expr(self.expr())
        }
    }

    /// compound_stmt = {declaration}* {stmt}* "}"
    fn compound_stmt(&mut self) -> ast::Stmt {
        let mut decls: Vec<ast::Decl> = Vec::new();
        while !self.equal("}") {
            if let TokenKind::Keyword(kwd) = self.tokens[self.cur].kind {
                if kwd == "long" {
                    self.cur += 1;

                    let name = if let TokenKind::Ident(ident) = self.tokens[self.cur].kind {
                        self.cur += 1;
                        self.skip(";");
                        String::from(ident)
                    } else {
                        panic!("variable name is required");
                    };
                    decls.push(ast::Decl {
                        var_type: ast::TypeExpr::Primitive(String::from("long")),
                        name,
                    })
                } else {
                    break;
                };
            } else {
                break;
            }
        }
        let mut stmts = Vec::new();
        while self.cur < self.tok_len && !self.equal("}") {
            stmts.push(self.stmt());
        }
        self.skip("}");
        ast::Stmt::Compound(decls, stmts)
    }

    /// expr       = assign
    fn expr(&mut self) -> ast::Expr {
        self.assign()
    }

    /// assign     = equality ("=" assign)?
    fn assign(&mut self) -> ast::Expr {
        let mut node = self.equality();
        if self.consume("=") {
            node = ast::Expr::Op(String::from("="), Vec::from([node, self.assign()]));
        }
        node
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

    /// primary = num
    ///         | ident ("(" ")")?
    ///         | "(" expr ")"
    fn primary(&mut self) -> ast::Expr {
        if self.consume("(") {
            let node = self.expr();
            self.skip(")");
            return node;
        }

        if let TokenKind::Ident(ident) = self.tokens[self.cur].kind {
            let node = ast::Expr::Id(String::from(ident));
            self.cur += 1;
            return node;
        }

        self.expect_number()
    }

    pub fn parse(&mut self) -> ast::Program {
        self.program()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
