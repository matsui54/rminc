use crate::rminc_ast;

pub fn str_to_ast(code: String) -> rminc_ast::Program {
    let def = rminc_ast::Def::Fun(
        String::from("main"),
        Vec::new(),
        rminc_ast::TypeExpr::Primitive(String::from("long")),
        rminc_ast::Stmt::Empty,
    );
    rminc_ast::Program {
        defs: Vec::from([def]),
    }
}
