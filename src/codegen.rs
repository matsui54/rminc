use crate::ast;
use std::collections::HashMap;

/// 「環境」を表す型
/// 変数名をindexとして、変数の入ったレジスタ(%rbpからのオフセット)を保存する
type Env = HashMap<String, u32>;

/// 引数を渡すのに使うレジスタ
static ARG_REGS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

/// 上記のenvなど、現在のソースコードが持っている状態を持つ構造体
struct Context<'a> {
    /// 環境
    env: &'a Env,
    /// v: 一時変数などとして、スタック上でどの位置から使うことができるのかを表す
    /// これは%rbpからのオフセットとして表すので、v=8のときは-8%(rbp)のようにして
    /// メモリにアクセスする。
    v: u32,
    /// labels: whileやifで用いるラベル (jmpなどで飛ぶ先) が他と被らないように
    /// 用いる変数。ラベルが必要になるたびにこの値をインクリメントする。
    labels: u32,
}

/// 四則演算などの計算を行なうアセンブリを生成する関数
/// オペランドが2つ (aとb) のときのアセンブリは以下のようになる
/// 1. bを計算する。bもExprなので、再帰的に計算する。
///     計算結果は%raxに入る
/// 2. %rax に入っている結果を一時変数としてスタック上に保存する
///     使えるスタックの領域は引数 v に入っていて、%rbpからのオフセットでアクセスする。
/// 3. aを計算する。
/// 4. aが%raxに、bがスタックに入っているので、演算子に応じた計算を行い、結果を%raxに代入する。
fn op_arithmetic_to_asm(inst: &str, exprs: &Vec<ast::Expr>, env: &Env, v: u32) -> String {
    match inst {
        "+" => {
            let tmp_mem = v;
            let asm1 = if exprs.len() == 2 {
                expr_to_asm(&exprs[1], env, v)
            } else if exprs.len() == 1 {
                String::from("  movq $0, %rax\n")
            } else {
                panic!("invalid num of operator {inst}")
            };
            asm1 + format!("  movq %rax, -{tmp_mem}(%rbp)\n").as_str()
                + expr_to_asm(&exprs[0], env, v + 8).as_str()
                + format!("  addq -{tmp_mem}(%rbp), %rax\n").as_str()
        }
        "-" => {
            let tmp_mem = v;
            let (asm1, asm0) = if exprs.len() == 2 {
                (
                    expr_to_asm(&exprs[1], env, v),
                    expr_to_asm(&exprs[0], env, v + 8),
                )
            } else if exprs.len() == 1 {
                (
                    expr_to_asm(&exprs[0], env, v),
                    String::from("  movq $0, %rax\n"),
                )
            } else {
                panic!("invalid num of operator {inst}")
            };
            asm1 + format!("  movq %rax, -{tmp_mem}(%rbp)\n").as_str()
                + asm0.as_str()
                + format!("  subq -{tmp_mem}(%rbp), %rax\n").as_str()
        }
        "*" => {
            let tmp_mem = v;
            expr_to_asm(&exprs[1], env, v)
                + format!("  movq %rax, -{tmp_mem}(%rbp)\n").as_str()
                + expr_to_asm(&exprs[0], env, v + 8).as_str()
                + format!("  imulq -{tmp_mem}(%rbp), %rax\n").as_str()
        }
        "/" => {
            let tmp_mem = v;
            expr_to_asm(&exprs[1], env, v)
                + format!("  movq %rax, -{tmp_mem}(%rbp)\n").as_str()
                + expr_to_asm(&exprs[0], env, v + 8).as_str()
                + format!("  movq -{tmp_mem}(%rbp), %rdi\n").as_str()
                + format!("  movq $0, %rdx\n").as_str()
                + format!("  idivq %rdi\n").as_str()
        }
        "%" => {
            let tmp_mem = v;
            // div命令は余りを%rdxに入れるので、%raxに移動する
            expr_to_asm(&exprs[1], env, v)
                + format!("  movq %rax, -{tmp_mem}(%rbp)\n").as_str()
                + expr_to_asm(&exprs[0], env, v + 8).as_str()
                + format!("  movq -{tmp_mem}(%rbp), %rdi\n").as_str()
                + format!("  movq $0, %rdx\n").as_str()
                + format!("  idivq %rdi\n").as_str()
                + format!("  movq %rdx, %rax\n").as_str()
        }
        "&" => {
            let tmp_mem = v;
            expr_to_asm(&exprs[1], env, v)
                + format!("  movq %rax, -{tmp_mem}(%rbp)\n").as_str()
                + expr_to_asm(&exprs[0], env, v + 8).as_str()
                + format!("  andq -{tmp_mem}(%rbp), %rax\n").as_str()
        }
        "!" => {
            expr_to_asm(&exprs[0], env, v + 8)
                + format!("  cmpq $0, %rax\n").as_str()
                + format!("  sete %al\n").as_str()
                + format!("  movzbl %al, %eax\n").as_str()
        }
        _ => {
            unreachable!()
        }
    }
}

/// 比較系の演算子を計算する
/// instはsetlやseteなどのconditional set命令
fn op_comp_to_asm(inst: &str, exprs: &Vec<ast::Expr>, env: &Env, v: u32) -> String {
    if exprs.len() != 2 {
        panic!("invalid num of operator {inst}")
    }
    let asm1 = expr_to_asm(&exprs[1], env, v);
    let asm0 = expr_to_asm(&exprs[0], env, v + 8);
    let m = v;
    asm1 + format!("  movq %rax, -{m}(%rbp)\n").as_str()
        + asm0.as_str()
        + format!("  cmpq -{m}(%rbp), %rax\n").as_str()
        + format!("  {inst} %cl\n").as_str()
        + format!("  movzbl %cl, %eax\n").as_str()
}

/// 演算系の処理
/// 結果は%raxに入れる
fn op_to_asm(op: String, exprs: &Vec<ast::Expr>, env: &Env, v: u32) -> String {
    match op.as_str() {
        // 変数への代入
        // envから値を探してくる
        "=" => {
            if exprs.len() != 2 {
                panic!("invalid num of operator +")
            }
            let (lhs, rhs) = (&exprs[0], &exprs[1]);
            let operand = match lhs {
                ast::Expr::Id(expr) => match env.get(expr) {
                    Some(expr) => *expr,
                    None => panic!("undefined variable {expr}"),
                },
                _ => panic!("Cannot assign value to non-variable"),
            };
            let asm1 = expr_to_asm(rhs, env, v);
            asm1 + format!("  movq %rax, -{operand}(%rbp)\n").as_str()
        }
        "+" | "-" | "*" | "/" | "%" | "!" | "&" => op_arithmetic_to_asm(op.as_str(), exprs, env, v),
        "==" => op_comp_to_asm("sete", exprs, env, v),
        "!=" => op_comp_to_asm("setne", exprs, env, v),
        "<" => op_comp_to_asm("setl", exprs, env, v),
        ">" => op_comp_to_asm("setg", exprs, env, v),
        "<=" => op_comp_to_asm("setle", exprs, env, v),
        ">=" => op_comp_to_asm("setge", exprs, env, v),
        _ => {
            panic!("invalid operator {op}")
        }
    }
}

/// Expr構造体からアセンブリを出力する関数。
/// exprを評価し、その結果を%raxレジスタに代入するようなアセンブリを生成する。
fn expr_to_asm(expr: &ast::Expr, env: &Env, v: u32) -> String {
    match expr {
        ast::Expr::IntLiteral(num) => format!("  movq ${num}, %rax\n"),
        ast::Expr::Id(id) => {
            let addr = match env.get(id) {
                Some(expr) => *expr,
                None => panic!("undefined variable {id}"),
            };
            format!("  movq -{addr}(%rbp), %rax\n")
        }
        ast::Expr::Op(op, exprs) => op_to_asm(op.to_string(), exprs, env, v),
        ast::Expr::Call(expr, exprs) => {
            let fn_name = match &*(*expr) {
                ast::Expr::Id(label) => label,
                _ => {
                    panic!("{:?} is not id", expr)
                }
            };
            let mut asm = String::from("");
            // rsp_offsetは関数呼び出しにあたってどれくらい%rspを動かすか
            // 変数や一時変数を置いてある領域が関数の中で上書きされると困るので、
            // rspを動かして退避させる。
            // また、引数が7つ以上ある場合もスタックに積む必要があるので、その分も確保する。
            let mut rsp_offset = v;
            if exprs.len() > ARG_REGS.len() {
                rsp_offset += ((exprs.len() - ARG_REGS.len()) as u32) * 8;
            }
            asm += format!("  subq ${rsp_offset}, %rsp\n").as_str();
            for i in 0..exprs.len() {
                let arg_asm = expr_to_asm(&exprs[i], env, v);
                asm += arg_asm.as_str();
                if i >= ARG_REGS.len() {
                    asm += format!("  movq %rax, {}(%rsp)\n", (i - ARG_REGS.len()) * 8).as_str();
                } else {
                    asm += format!("  movq %rax, {}\n", ARG_REGS[i]).as_str();
                }
            }
            asm += format!("  call {fn_name}@PLT\n").as_str();
            // 伸ばしたrspをもとに戻す
            asm += format!("  addq ${rsp_offset}, %rsp\n").as_str();
            asm
        }
        ast::Expr::Paren(expr) => expr_to_asm(&(*expr), env, v),
    }
}

/// statementをアセンブリに変換する
fn stmt_to_asm(stmt: ast::Stmt, context: &mut Context) -> String {
    match stmt {
        ast::Stmt::Empty => String::from(""),
        ast::Stmt::Continue => String::from(""),
        ast::Stmt::Break => String::from(""),
        // expr_to_asmは%raxに結果を書くのでそのままretqする。
        // %rbpも戻す
        ast::Stmt::Return(expr) => format!(
            "{}  popq %rbp\n  retq\n",
            expr_to_asm(&expr, &context.env, context.v)
        ),
        ast::Stmt::Expr(expr) => expr_to_asm(&expr, &context.env, context.v),
        // 波括弧で囲まれた部分
        // この中で宣言された変数はこの中でしか使えないが、
        // スコープ外で定義された変数にはアクセスできる。
        // envをコピーすることによって、スコープの概念を実現する。
        ast::Stmt::Compound(decls, stmts) => {
            let mut env = context.env.clone();
            for decl in decls {
                env.insert(decl.name, context.v);
                context.v += 8;
            }
            stmts
                .into_iter()
                .map(|stmt| {
                    stmt_to_asm(
                        stmt,
                        &mut Context {
                            env: &env,
                            v: context.v,
                            labels: context.labels,
                        },
                    )
                })
                .collect::<Vec<_>>()
                .join("")
        }
        // if文
        // 条件を評価し、結果が0ならelse節か最後までジャンプ
        ast::Stmt::If(expr, stmt, stmt_else) => {
            let cond_asm = expr_to_asm(&expr, context.env, context.v);
            let body = stmt_to_asm(*stmt, context);
            context.labels += 1;
            match stmt_else {
                Some(st) => {
                    let label = context.labels;
                    cond_asm
                        + "  cmpq $0, %rax\n"
                        + format!("  je .LE{label}\n").as_str()
                        + body.as_str()
                        + format!("  jmp .LL{label}\n").as_str()
                        + format!(".LE{label}:\n").as_str()
                        + stmt_to_asm(*st, context).as_str()
                        + format!(".LL{label}:\n").as_str()
                }
                None => {
                    cond_asm
                        + "  cmpq $0, %rax\n"
                        + format!("  je .LE{}\n", context.labels).as_str()
                        + body.as_str()
                        + format!(".LE{}:\n", context.labels).as_str()
                }
            }
        }
        // while文
        // 波括弧の中の処理→条件の評価
        // という順序で書き、まずは条件の評価にジャンプする
        // 続ける条件が満たされていれば、上に戻って処理を継続する。
        ast::Stmt::While(expr, stmt) => {
            let cond_asm = expr_to_asm(&expr, context.env, context.v);
            let body_asm = stmt_to_asm(*stmt, context);
            context.labels += 1;
            format!(
                r#"
  jmp .LC{0}
.LS{0}:{body_asm}
.LC{0}:{cond_asm}
  cmpq $0, %rax
  jne .LS{0}
"#,
                context.labels
            )
        },
        ast::Stmt::For(expr0, expr1, expr2, stmt) => {
            unimplemented!()
        }
    }
}

/// 一つの関数をアセンブリに変換する
fn def_to_asm(fun: ast::Def) -> String {
    match fun {
        ast::Def::Fun(str, decls, _, stmt) => {
            // 決まり文句
            // rbpはcollee saveなので、スタックに積む
            let prologue = format!(
                r#"
  .globl	{str}
  .type	{str}, @function
{str}:
  .cfi_startproc
  endbr64
  pushq %rbp
  movq %rsp, %rbp
"#
            );
            let epilogue = r#"
  .cfi_endproc
"#;
            // vはスタック上でどの場所から空いているかを表す
            let mut v = 8; // pushq %rbpをしているので、8からスタート
            let mut env = Env::new(); // envは変数を保存する変数
            let mut asm = String::from("");

            // 変数の宣言を処理
            // envのHashMapに追加していく
            // vも8ずつ足していく
            for (i, decl) in decls.iter().enumerate() {
                env.insert(decl.name.clone(), v);
                if i >= ARG_REGS.len() {
                    // 引数が7つ以上ある場合はスタックから持ってくる
                    asm +=
                        format!("  movq {}(%rsp), %rax\n", (i - ARG_REGS.len() + 2) * 8).as_str();
                    asm += format!("  movq %rax, -{v}(%rbp)\n").as_str();
                } else {
                    asm += format!("  movq {}, -{v}(%rbp)\n", ARG_REGS[i]).as_str();
                }
                v += 8;
            }

            // stmtの処理
            asm += stmt_to_asm(
                stmt,
                &mut Context {
                    env: &env,
                    v,
                    labels: 0,
                },
            )
            .as_str();
            prologue + asm.as_str() + epilogue
        }
    }
}

/// astからアセンブリを出力する関数
/// 関数ごとにアセンブリを生成して結合する。
pub fn ast_to_asm_program(_program: ast::Program) -> String {
    _program
        .defs
        .into_iter()
        .map(|def| def_to_asm(def))
        .collect::<Vec<_>>()
        .join("")
}

