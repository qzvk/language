//! Internal representation type

use parser::{Ast, Builtin, Expr as AstExpr, FunctionId};

/// An internal representation of a program, used during evaluation.
pub struct Ir(Expr);

impl Ir {
    /// Convert an abstract syntax tree into an internal representation.
    pub fn new(ast: Ast) -> Result<Self, Error> {
        let main = ast.get("main").unwrap().clone();
        let expr = Expr::new(main);
        Ok(Self(expr))
    }

    /// The main function of a program.
    pub fn into_main(self) -> Expr {
        self.0
    }
}

pub enum Expr {
    Integer(i64),
    Apply(Function, Vec<Self>),
}

pub enum Function {
    Builtin(Builtin),
    User(FunctionId),
}

impl Expr {
    pub fn new(expr: AstExpr) -> Self {
        match expr {
            AstExpr::Integer(value) => Self::Integer(value),

            AstExpr::Apply(info) => {
                let (left, right) = *info;
                Self::new_apply(left, right)
            }

            AstExpr::Function(id) => Self::Apply(Function::User(id), Vec::new()),

            _ => todo!(),
        }
    }

    fn new_apply(mut left: AstExpr, right: AstExpr) -> Self {
        // NOTE: The way this function 'flattens' the AST into a list of arguments means that the
        // arguments will appear in REVERSE ordering.

        let function: Function;
        let mut args = Vec::new();

        args.push(Self::new(right));

        loop {
            match left {
                AstExpr::Apply(info) => {
                    let (new_left, right) = *info;
                    args.push(Self::new(right));
                    left = new_left;
                }

                AstExpr::Builtin(builtin) => {
                    function = Function::Builtin(builtin);
                    break;
                }

                _ => todo!(),
            }
        }

        // TODO: Perform additional checks here (argument count, type correctness, etc.)

        Self::Apply(function, args)
    }
}

#[derive(Debug)]
pub struct Error {}
