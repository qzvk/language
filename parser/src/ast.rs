use crate::parse_tree::{AssignmentSeq, Expr as ParseExpr, Operator};
use lexer::Span;
use std::collections::HashMap;

/// An abstract syntax tree
#[derive(Debug, PartialEq, Eq)]
pub struct Ast<'a> {
    functions: HashMap<&'a str, Expr>,
}

impl<'a> Ast<'a> {
    /// Create an AST from an assignment-seq parse tree node
    pub fn from_assignment_seq(tree: AssignmentSeq<'a>) -> Result<Self, Vec<Error>> {
        let mut function_names = Vec::with_capacity(tree.assignments().len());
        for assignment in tree.assignments() {
            function_names.push(assignment.name());
        }

        if function_names.is_empty() {
            return Err(vec![Error::EmptyInput]);
        }

        let mut functions = HashMap::new();
        let mut errors = Vec::new();

        for assignment in tree.into_assignments() {
            let (name, args, body) = assignment.into_parts();

            let name = name.source();
            match Expr::from_expr(body, &args, &function_names) {
                Ok(body) => {
                    functions.insert(name, body);
                }
                Err(error) => {
                    errors.extend(error);
                }
            }
        }

        if errors.is_empty() {
            Ok(Self::new(functions))
        } else {
            Err(errors)
        }
    }

    /// Create an empty AST
    pub fn new(functions: HashMap<&'a str, Expr>) -> Self {
        Self { functions }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Integer(i64),
    Argument(ArgumentId),
    Function(FunctionId),
    Builtin(Builtin),
    Apply(Box<(Self, Self)>),
}

impl Expr {
    pub fn from_expr<'a>(
        expr: ParseExpr<'a>,
        arguments: &[Span],
        function_names: &[Span],
    ) -> Result<Self, Vec<Error<'a>>> {
        match expr {
            ParseExpr::Integer(span) => {
                if let Ok(value) = span.source().parse::<i64>() {
                    Ok(Self::Integer(value))
                } else {
                    Err(vec![Error::BadIntegerLiteral(span)])
                }
            }

            ParseExpr::Ident(span) => {
                let value = span.source();
                if let Some(id) = function_names.iter().position(|n| n.source() == value) {
                    Ok(Self::Function(FunctionId(id as u32)))
                } else if let Some(id) = arguments.iter().position(|n| n.source() == value) {
                    Ok(Self::Argument(ArgumentId(id as u32)))
                } else {
                    Err(vec![Error::UnknownIdent(span)])
                }
            }

            ParseExpr::Operator(op, _, children) => {
                let (a, b) = *children;

                let first = Self::operator(op);
                let second = Self::from_expr(a, arguments, function_names);
                let third = Self::from_expr(b, arguments, function_names);

                match (second, third) {
                    (Ok(s), Ok(t)) => Ok(Self::apply(Self::apply(first, s), t)),

                    (Ok(_), Err(error)) | (Err(error), Ok(_)) => Err(error),

                    (Err(mut errors), Err(more_errors)) => {
                        errors.extend(more_errors);
                        Err(errors)
                    }
                }
            }

            ParseExpr::Apply(boxed_info) => {
                let (left, right) = *boxed_info;

                let left = Self::from_expr(left, arguments, function_names);
                let right = Self::from_expr(right, arguments, function_names);

                match (left, right) {
                    (Ok(l), Ok(r)) => Ok(Self::apply(l, r)),

                    (Ok(_), Err(errors)) | (Err(errors), Ok(_)) => Err(errors),

                    (Err(mut errors), Err(more_errors)) => {
                        errors.extend(more_errors);
                        Err(errors)
                    }
                }
            }
        }
    }

    pub fn operator(operator: Operator) -> Self {
        let builtin = Builtin::from_operator(operator);
        Self::Builtin(builtin)
    }

    pub fn apply(left: Self, right: Self) -> Self {
        Self::Apply(Box::new((left, right)))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FunctionId(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ArgumentId(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Builtin {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Builtin {
    pub fn from_operator(op: Operator) -> Self {
        match op {
            Operator::Add => Self::Add,
            Operator::Subtract => Self::Subtract,
            Operator::Multiply => Self::Multiply,
            Operator::Divide => Self::Divide,
        }
    }
}

/// An error encountered during AST generation
#[derive(Debug, PartialEq, Eq)]
pub enum Error<'a> {
    /// The input was empty
    EmptyInput,

    /// Integer literal could not be parsed, likely because it was too large
    BadIntegerLiteral(Span<'a>),

    /// An identifier which is not a function name or argument
    UnknownIdent(Span<'a>),
}

impl<'a> std::fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::EmptyInput => f.write_str("error: the input was empty\n"),

            Error::BadIntegerLiteral(span) => writeln!(
                f,
                "{}:{}: error: integer literal {} is too large",
                span.line() + 1,
                span.column() + 1,
                span.source(),
            ),

            Error::UnknownIdent(span) => writeln!(
                f,
                "{}:{}: error: unknown identifier {:?}",
                span.line() + 1,
                span.column() + 1,
                span.source(),
            ),
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

#[cfg(test)]
mod tests {
    use super::{AssignmentSeq, Ast, Builtin, Error, Expr, FunctionId};
    use crate::{
        ast::ArgumentId,
        parse_tree::{Assignment, Expr as PExpr, Operator},
    };
    use lexer::Span;
    use std::collections::HashMap;

    fn make_ast<const N: usize>(items: [(&str, Expr); N]) -> Ast {
        let mut hash_map = HashMap::new();

        for (key, value) in items {
            let old = hash_map.insert(key, value);
            assert!(old.is_none());
        }

        Ast::new(hash_map)
    }

    macro_rules! example {
        ($name:ident, $input:expr, $expected:expr $(,)?) => {
            #[test]
            fn $name() {
                let input: super::AssignmentSeq = $input;
                let expected: super::Ast = make_ast($expected);
                let actual: super::Ast = super::Ast::from_assignment_seq(input).unwrap();
                assert_eq!(expected, actual);
            }
        };
    }

    macro_rules! error_example {
        ($name:ident, $input:expr, $expected:expr $(,)?) => {
            #[test]
            fn $name() {
                let input: super::AssignmentSeq = $input;
                let expected: ::std::vec::Vec<super::Error> = Vec::from($expected);
                let actual: ::std::vec::Vec<super::Error> =
                    super::Ast::from_assignment_seq(input).unwrap_err();
                assert_eq!(expected, actual);
            }
        };
    }

    // main = 1234;
    example! {
        simple,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                PExpr::Integer(Span::new(0, 7, "1234")),
            ),
        ]),
        [("main", Expr::Integer(1234))],
    }

    // main = 3 * 4 + 2 - 8 / 4
    example! {
        expression,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                PExpr::Operator(Operator::Add, Span::new(0, 15, "+"), Box::new((
                    PExpr::Operator(Operator::Multiply, Span::new(0, 9, "*"), Box::new((
                        PExpr::Integer(Span::new(0, 8, "3")),
                        PExpr::Integer(Span::new(0, 12, "4")),
                    ))),
                    PExpr::Operator(Operator::Subtract, Span::new(0, 20, "-"), Box::new((
                        PExpr::Integer(Span::new(0, 18, "2")),
                        PExpr::Operator(Operator::Divide, Span::new(0, 22, "/"), Box::new((
                            PExpr::Integer(Span::new(0, 20, "8")),
                            PExpr::Integer(Span::new(0, 24, "4")),
                        ))),
                    ))),
                ))),
            ),
        ]),
        [(
            "main",
            Expr::apply(
                Expr::apply(
                    Expr::Builtin(Builtin::Add),
                    Expr::apply(
                        Expr::apply(
                            Expr::Builtin(Builtin::Multiply),
                            Expr::Integer(3),
                        ),
                        Expr::Integer(4),
                    )
                ),
                Expr::apply(
                    Expr::apply(
                        Expr::Builtin(Builtin::Subtract),
                        Expr::Integer(2),
                    ),
                    Expr::apply(
                        Expr::apply(
                            Expr::Builtin(Builtin::Divide),
                            Expr::Integer(8),
                        ),
                        Expr::Integer(4),
                    )
                )
            ),
        )],
    }

    // f = 1;
    // main = f;
    example! {
        function_call,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "f"),
                Vec::new(),
                PExpr::Integer(Span::new(0, 4, "1")),
            ),
            Assignment::new(
                Span::new(0, 7, "main"),
                Vec::new(),
                PExpr::Ident(Span::new(0, 14, "f")),
            ),
        ]),
        [
            ("f", Expr::Integer(1)),
            ("main", Expr::Function(FunctionId(0))),
        ],
    }

    // id x = x;
    // main = id 5;
    example! {
        single_argument,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "id"),
                vec![
                    Span::new(0, 3, "x"),
                ],
                PExpr::Ident(Span::new(0, 7, "x")),
            ),
            Assignment::new(
                Span::new(1, 0, "main"),
                Vec::new(),
                PExpr::Apply(Box::new((
                    PExpr::Ident(Span::new(1, 7, "id")),
                    PExpr::Integer(Span::new(1, 10, "5")),
                ))),
            ),
        ]),
        [
            ("id", Expr::Argument(ArgumentId(0))),
            (
                "main",
                Expr::apply(
                    Expr::Function(FunctionId(0)),
                    Expr::Integer(5),
                ),
            ),
        ],
    }

    error_example! {
        empty_input,
        AssignmentSeq::new(Vec::new()),
        [Error::EmptyInput],
    }

    // main = 99999999999999999999;
    error_example! {
        overlong_integer,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                PExpr::Integer(Span::new(0, 7, "99999999999999999999")),
            ),
        ]),
        [
            Error::BadIntegerLiteral(Span::new(0, 7, "99999999999999999999")),
        ],
    }

    // main = unknown;
    error_example! {
        unknown_identifier,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                PExpr::Ident(Span::new(0, 7, "unknown")),
            ),
        ]),
        [
            Error::UnknownIdent(Span::new(0, 7, "unknown")),
        ],
    }

    // main = unknown + 99999999999999999999;
    error_example! {
        multiple_errors_under_operator,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                PExpr::Operator(
                    Operator::Add,
                    Span::new(0, 15, "+"),
                    Box::new((
                        PExpr::Ident(Span::new(0, 7, "unknown")),
                        PExpr::Integer(Span::new(0, 17, "99999999999999999999")),
                    ))
                ),
            ),
        ]),
        [
            Error::UnknownIdent(Span::new(0, 7, "unknown")),
            Error::BadIntegerLiteral(Span::new(0, 17, "99999999999999999999")),
        ],
    }

    // main = unknown 99999999999999999999;
    error_example! {
        multiple_errors_under_application,
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                PExpr::Apply(Box::new((
                    PExpr::Ident(Span::new(0, 7, "unknown")),
                    PExpr::Integer(Span::new(0, 15, "99999999999999999999")),
                ))),
            ),
        ]),
        [
            Error::UnknownIdent(Span::new(0, 7, "unknown")),
            Error::BadIntegerLiteral(Span::new(0, 15, "99999999999999999999")),
        ],
    }
}
