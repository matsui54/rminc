use crate::ast;
use crate::tokenize::{Token, TokenKind};
use crate::util;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    cur: usize,
    code: &'a str,
}

impl Parser<'_> {
    pub fn new<'a>(tokens: Vec<Token<'a>>, code: &'a str) -> Parser<'a> {
        Parser {
            tokens,
            cur: 0,
            code,
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

    fn equal(&mut self, op: &str) -> bool {
        if let TokenKind::Reserved(str) = self.tokens[self.cur].kind {
            if str == op {
                return true;
            }
        }
        false
    }

    fn skip(&mut self, op: &str) {
        if !self.consume(op) {
            util::error_tok(
                self.code,
                &self.tokens[self.cur],
                format!("{} is expected", op).as_str(),
            );
        }
    }

    fn expect_number(&mut self) -> ast::Expr {
        let current = self.tokens[self.cur].at;
        if let TokenKind::Num(num) = self.tokens[self.cur].kind {
            self.cur += 1;
            ast::Expr {
                kind: ast::ExprKind::IntLiteral(num),
                at: current,
            }
        } else {
            util::error_tok(
                self.code,
                &self.tokens[self.cur],
                format!("number is expected").as_str(),
            )
        }
    }

    /// program    = stmt*
    fn program(&mut self) -> ast::Program {
        let mut stmts = Vec::new();
        loop {
            if let TokenKind::EOF = self.tokens[self.cur].kind {
                break;
            } else {
                stmts.push(self.stmt());
            }
        }
        ast::Program {
            defs: Vec::from([ast::Def::Fun(
                String::from("main"),
                Vec::new(),
                ast::TypeExpr::Primitive(String::from("long")),
                ast::Stmt {
                    kind: ast::StmtKind::Compound(Vec::new(), stmts),
                    at: 0,
                },
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
        let current = self.tokens[self.cur].at;
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
                    ast::Stmt {
                        kind: ast::StmtKind::If(expr, stmt, stmt_else),
                        at: current,
                    }
                }
                "while" => {
                    self.skip("(");
                    let expr = self.expr();
                    self.skip(")");
                    let stmt = Box::new(self.stmt());
                    ast::Stmt {
                        kind: ast::StmtKind::While(expr, stmt),
                        at: current,
                    }
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
                    ast::Stmt {
                        kind: ast::StmtKind::For(expr0, expr1, expr2, Box::new(self.stmt())),
                        at: current,
                    }
                }
                "return" => {
                    let stmt = ast::StmtKind::Return(self.expr());
                    self.skip(";");
                    ast::Stmt {
                        kind: stmt,
                        at: current,
                    }
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
            ast::Stmt {
                kind: ast::StmtKind::Empty,
                at: 0,
            }
        } else {
            let node = ast::Stmt {
                kind: ast::StmtKind::Expr(self.expr()),
                at: self.tokens[self.cur].at,
            };
            self.skip(";");
            node
        }
    }

    /// compound_stmt = {declaration}* {stmt}* "}"
    fn compound_stmt(&mut self) -> ast::Stmt {
        let mut decls: Vec<ast::Decl> = Vec::new();
        let current = self.tokens[self.cur].at;
        while !self.equal("}") {
            if let TokenKind::Keyword(kwd) = self.tokens[self.cur].kind {
                if kwd == "long" {
                    self.cur += 1;

                    let name = if let TokenKind::Ident(ident) = self.tokens[self.cur].kind {
                        self.cur += 1;
                        self.skip(";");
                        String::from(ident)
                    } else {
                        util::error_tok(
                            self.code,
                            &self.tokens[self.cur],
                            "variable name is required",
                        )
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
        while !self.consume("}") {
            stmts.push(self.stmt());
        }
        ast::Stmt {
            kind: ast::StmtKind::Compound(decls, stmts),
            at: current,
        }
    }

    /// expr       = assign
    fn expr(&mut self) -> ast::Expr {
        self.assign()
    }

    /// assign     = equality ("=" assign)?
    fn assign(&mut self) -> ast::Expr {
        let mut node = self.equality();
        if self.consume("=") {
            node = ast::Expr {
                kind: ast::ExprKind::Op(String::from("="), Vec::from([node, self.assign()])),
                at: self.tokens[self.cur - 1].at,
            };
        }
        node
    }

    /// equality   = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> ast::Expr {
        let mut node = self.relational();
        loop {
            if self.consume("==") {
                node = ast::Expr {
                    kind: ast::ExprKind::Op(
                        String::from("=="),
                        Vec::from([node, self.relational()]),
                    ),
                    at: self.tokens[self.cur - 1].at,
                };
                continue;
            }
            if self.consume("!=") {
                node = ast::Expr {
                    kind: ast::ExprKind::Op(
                        String::from("!="),
                        Vec::from([node, self.relational()]),
                    ),
                    at: self.tokens[self.cur - 1].at,
                };
                continue;
            }
            return node;
        }
    }

    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> ast::Expr {
        let mut node = self.add();
        'outer: loop {
            for pat in ["<", "<=", ">", ">="] {
                if self.consume(pat) {
                    node = ast::Expr {
                        kind: ast::ExprKind::Op(String::from(pat), Vec::from([node, self.add()])),
                        at: self.tokens[self.cur - 1].at,
                    };
                    continue 'outer;
                }
            }
            return node;
        }
    }

    /// add        = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> ast::Expr {
        let mut node = self.mul();
        loop {
            if self.consume("+") {
                node = ast::Expr {
                    kind: ast::ExprKind::Op(String::from("+"), Vec::from([node, self.mul()])),
                    at: self.tokens[self.cur - 1].at,
                };
                continue;
            }
            if self.consume("-") {
                node = ast::Expr {
                    kind: ast::ExprKind::Op(String::from("-"), Vec::from([node, self.mul()])),
                    at: self.tokens[self.cur - 1].at,
                };
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
                node = ast::Expr {
                    kind: ast::ExprKind::Op(String::from("*"), Vec::from([node, self.unary()])),
                    at: self.tokens[self.cur - 1].at,
                };
                continue;
            }
            if self.consume("/") {
                node = ast::Expr {
                    kind: ast::ExprKind::Op(String::from("/"), Vec::from([node, self.unary()])),
                    at: self.tokens[self.cur - 1].at,
                };
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
            return ast::Expr {
                kind: ast::ExprKind::Op(
                    String::from("-"),
                    Vec::from([
                        ast::Expr {
                            kind: ast::ExprKind::IntLiteral(0),
                            at: self.tokens[self.cur - 1].at,
                        },
                        self.unary(),
                    ]),
                ),
                at: self.tokens[self.cur - 1].at,
            };
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
            let node = ast::Expr {
                kind: ast::ExprKind::Id(String::from(ident)),
                at: self.tokens[self.cur].at,
            };
            self.cur += 1;
            return node;
        }

        self.expect_number()
    }

    pub fn parse(&mut self) -> ast::Program {
        self.program()
    }
}
