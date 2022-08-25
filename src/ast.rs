#[derive(Debug)]
pub enum TypeExpr {
    Primitive(String)		// type name (always "long", for now)
}

/* variable declaration:

   long x; -> Decl{var_type=TypeExpr::Primitive("long"), name="x"}
   also (ab)used to represent a function parameter */

#[derive(Debug)]
pub struct Decl {
    pub var_type : TypeExpr,
    pub name : String
}

#[derive(Debug)]
/* expression */
pub enum Expr {
    IntLiteral(i64),            /* 1, 2, 3, ... */
    Id(String),                 /* x, y, z, ... */
    Op(String, Vec<Expr>),      /* -x, x - y, ... */
    Call(Box<Expr>, Vec<Expr>), /* f(1, 2, 3) */
    Paren(Box<Expr>)            /* (x + y) */
}

/* statement */

#[derive(Debug)]
pub enum Stmt {
    Empty,                      /* ; */
    Continue,                   /* continue; */
    Break,                      /* break; */
    Return(Expr),               /* return e; */
    Expr(Expr),                 /* f(x); */
    Compound(Vec<Decl>, Vec<Stmt>), /* { int x; return x + 1; } */
    If(Expr, Box<Stmt>, Option<Box<Stmt>>), /* if (expr) stmt [else stmt] */
    While(Expr, Box<Stmt>)                  /* while (expr) stmt */
}

#[derive(Debug)]
/* toplevel definition */
pub enum Def {
    /* function definition
     e.g., long f(long x, long y) { return x; } */
    Fun(String, Vec<Decl>, TypeExpr, Stmt)
}

#[derive(Debug)]
/* program is just a list of definitions */
pub struct Program {
    pub defs : Vec<Def>
}

