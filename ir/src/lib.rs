#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Internal representation types used during evaluation.

mod ir;

pub use crate::ir::Ir;

use crate::ir::{Expr, Function};
use parser::Builtin;

/// A final value produced by evaluation of [`Ir`].
#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    /// An integer value.
    Integer(i64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self::Integer(value) = self;
        write!(f, "{value}")
    }
}

/// Perform evaluation of an intermediate representation.
pub fn evaluate(ir: Ir) -> Result<Value, Error> {
    let expr = ir.into_main();
    evaluate_expr(expr)
}

fn evaluate_expr(expr: Expr) -> Result<Value, Error> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(value)),
        Expr::Apply(function, args) => evaluate_apply(function, args),
    }
}

fn evaluate_apply(function: Function, args: Vec<Expr>) -> Result<Value, Error> {
    match function {
        Function::Builtin(builtin) => evaluate_builtin(builtin, args),
        Function::User(_) => todo!(),
    }
}

fn evaluate_builtin(builtin: Builtin, mut args: Vec<Expr>) -> Result<Value, Error> {
    // TODO: Temporary 'get the two arguments' lines. Will be replaced when proper function
    // evaluation is added.
    let left = evaluate_expr(args.pop().unwrap())?;
    let right = evaluate_expr(args.pop().unwrap())?;

    let Value::Integer(l) = left;
    let Value::Integer(r) = right;

    let value = match builtin {
        Builtin::Add => l + r,
        Builtin::Subtract => l - r,
        Builtin::Multiply => l * r,
        Builtin::Divide => l / r,
    };

    Ok(Value::Integer(value))
}

/// An error encountered during evaluation.
#[derive(Debug)]
pub struct Error {}
