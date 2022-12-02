use lexer::{Span, TokenKind};
use std::iter::Peekable;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operator {
    pub fn infix_binding_powers(self) -> (u8, u8) {
        match self {
            Operator::Add | Operator::Subtract => (1, 2),
            Operator::Multiply | Operator::Divide => (3, 4),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Integer(Span<'a>),
    Ident(Span<'a>),
    Operator(Operator, Span<'a>, Box<(Self, Self)>),
    Apply(Box<(Self, Self)>),
}

impl<'a> Expr<'a> {
    pub fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, Vec<Error<'a>>>
    where
        I: Iterator<Item = (TokenKind, Span<'a>)>,
    {
        Self::parse_with_power(tokens, 0)
    }

    fn parse_with_power<I>(
        tokens: &mut Peekable<I>,
        minimum_binding_power: u8,
    ) -> Result<Self, Vec<Error<'a>>>
    where
        I: Iterator<Item = (TokenKind, Span<'a>)>,
    {
        let mut lhs = match tokens.next() {
            Some((TokenKind::Integer, span)) => Expr::Integer(span),
            Some((TokenKind::Ident, span)) => Expr::Ident(span),
            Some((TokenKind::OpenParen, _)) => {
                let lhs = Self::parse_with_power(tokens, 0)?;
                assert!(matches!(tokens.next(), Some((TokenKind::CloseParen, _))));
                lhs
            }
            Some((_, span)) => return Err(vec![Error::UnknownExprStart(span)]),
            None => return Err(vec![Error::EofExpr]),
        };

        loop {
            let (operator, span) = match tokens.peek() {
                Some(&(TokenKind::Plus, span)) => (Operator::Add, span),
                Some(&(TokenKind::Minus, span)) => (Operator::Subtract, span),
                Some(&(TokenKind::Asterisk, span)) => (Operator::Multiply, span),
                Some(&(TokenKind::Slash, span)) => (Operator::Divide, span),
                Some(&(TokenKind::CloseParen, _)) => break,
                Some(&(TokenKind::Ident, span)) => {
                    tokens.next();
                    lhs = Expr::Apply(Box::new((lhs, Expr::Ident(span))));
                    continue;
                }
                Some(&(TokenKind::Integer, span)) => {
                    tokens.next();
                    lhs = Expr::Apply(Box::new((lhs, Expr::Integer(span))));
                    continue;
                }
                Some((TokenKind::OpenParen, _)) => {
                    tokens.next();
                    let rhs = Self::parse_with_power(tokens, 0)?;
                    assert!(matches!(tokens.next(), Some((TokenKind::CloseParen, _))));
                    lhs = Expr::Apply(Box::new((lhs, rhs)));
                    continue;
                }
                _ => break,
            };

            let (left_binding_power, right_binding_power) = operator.infix_binding_powers();
            if left_binding_power < minimum_binding_power {
                break;
            }

            tokens.next();

            let rhs = Self::parse_with_power(tokens, right_binding_power)?;
            lhs = Expr::Operator(operator, span, Box::new((lhs, rhs)));
        }

        Ok(lhs)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assignment<'a> {
    name: Span<'a>,
    args: Vec<Span<'a>>,
    body: Expr<'a>,
}

impl<'a> Assignment<'a> {
    pub fn new(name: Span<'a>, args: Vec<Span<'a>>, body: Expr<'a>) -> Self {
        Self { name, args, body }
    }

    pub fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, Vec<Error<'a>>>
    where
        I: Iterator<Item = (TokenKind, Span<'a>)>,
    {
        let name = match tokens.next() {
            Some((TokenKind::Ident, span)) => span,
            Some((TokenKind::Equals, span)) => {
                return Err(vec![Error::MissingAssignmentName(span)])
            }
            Some((_, span)) => return Err(vec![Error::BadAssignmentName(span)]),
            None => return Err(vec![Error::EofAssignment]),
        };

        let mut errors = Vec::new();
        let mut args = Vec::new();

        loop {
            match tokens.next() {
                Some((TokenKind::Equals, _)) => break,
                Some((TokenKind::Ident, span)) => args.push(span),
                Some((_, span)) => errors.push(Error::UnknownAssignmentArgument(span)),
                None => {
                    errors.push(Error::EofAssignmentEquals);
                    return Err(errors);
                }
            }
        }

        let body = match Expr::parse(tokens) {
            Ok(o) => o,
            Err(e) => {
                errors.extend(e);
                return Err(errors);
            }
        };

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(Assignment::new(name, args, body))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignmentSeq<'a> {
    assignments: Vec<Assignment<'a>>,
}

impl<'a> AssignmentSeq<'a> {
    pub fn new(assignments: Vec<Assignment<'a>>) -> Self {
        Self { assignments }
    }

    pub fn parse<I>(tokens: &mut Peekable<I>) -> Result<Self, Vec<Error<'a>>>
    where
        I: Iterator<Item = (TokenKind, Span<'a>)>,
    {
        let mut assignments = Vec::new();
        let mut errors = Vec::new();

        loop {
            if tokens.peek().is_none() {
                break;
            } else {
                match Assignment::parse(tokens) {
                    Ok(assignment) => {
                        assignments.push(assignment);
                        assert!(matches!(
                            tokens.next(),
                            None | Some((TokenKind::Semicolon, _))
                        ));
                    }
                    Err(e) => {
                        while !matches!(tokens.next(), None | Some((TokenKind::Semicolon, _))) {
                            // Discard tokens until EOF or `;`.
                        }
                        errors.extend(e)
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(Self::new(assignments))
        } else {
            Err(errors)
        }
    }
}

/// An error found during parse tree generation.
#[derive(Debug, PartialEq, Eq)]
pub enum Error<'a> {
    /// An unknown token started an expression
    UnknownExprStart(Span<'a>),

    /// Expected an expression, saw EOF
    EofExpr,

    /// Expected an assignment, saw EOF
    EofAssignment,

    /// An assignment name was not an identifier
    BadAssignmentName(Span<'a>),

    /// Expected an assignment name, saw an equals
    MissingAssignmentName(Span<'a>),

    /// Saw something other than an identifier in assignment arguments
    UnknownAssignmentArgument(Span<'a>),

    /// Expected an assignment's '=', saw EOF
    EofAssignmentEquals,
}

impl<'a> std::fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnknownExprStart(span) => {
                writeln!(
                    f,
                    "error: {}:{}: expected an expression, saw {:?}",
                    span.line() + 1,
                    span.column() + 1,
                    span.source(),
                )
            }
            Error::EofExpr => writeln!(f, "error: expected an expression, saw EOF"),
            Error::EofAssignment => writeln!(f, "error: expected an assignment, saw EOF"),
            Error::BadAssignmentName(span) => {
                writeln!(
                    f,
                    "error: {}:{}: an assignment's name must be an identifier, saw {:?} instead",
                    span.line() + 1,
                    span.column() + 1,
                    span.source(),
                )
            }
            Error::MissingAssignmentName(span) => {
                writeln!(
                    f,
                    "error: {}:{}: expected an assignment's name, saw {:?} instead",
                    span.line() + 1,
                    span.column() + 1,
                    span.source(),
                )
            }
            Error::UnknownAssignmentArgument(span) => {
                writeln!(
                    f,
                    "error: {}:{}: argument names must be identifiers, saw {:?} instead",
                    span.line() + 1,
                    span.column() + 1,
                    span.source(),
                )
            }
            Error::EofAssignmentEquals => writeln!(f, "error: expected an '=', saw EOF"),
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

#[cfg(test)]
mod tests {
    use super::{Assignment, AssignmentSeq, Error, Expr, Operator};
    use lexer::{Span, TokenKind};

    macro_rules! example {
        ($name:ident, $type:ty, [ $($input:expr),* $(,)? ], $expected:expr $(,)?) => {
            #[test]
            fn $name() {
                let mut input = [$($input),*]
                    .into_iter()
                    .peekable();
                let expected = $expected;
                let actual = <$type>::parse(&mut input).unwrap();
                assert_eq!(expected, actual);
            }
        };
    }

    macro_rules! error_example {
        (
            $name:ident,
            $type:ty,
            [ $($input:expr),* $(,)? ],
            [ $($expected:expr),* $(,)? ],
            $(,)?
        ) => {
            #[test]
            fn $name() {
                let mut input = [$($input),*]
                    .into_iter()
                    .peekable();
                let expected: Vec<_> = [$($expected),*]
                    .into_iter()
                    .collect();
                let actual = <$type>::parse(&mut input).unwrap_err();
                assert_eq!(expected, actual);
            }
        };
    }

    example! {
        integer,
        Expr,
        [(TokenKind::Integer, Span::new(0, 0, "12345"))],
        Expr::Integer(Span::new(0, 0, "12345")),
    }

    example! {
        identifier,
        Expr,
        [
            (TokenKind::Ident, Span::new(0, 1, "hello"))
        ],
        Expr::Ident(Span::new(0, 1, "hello")),
    }

    example! {
        operators,
        Expr,
        [
            (TokenKind::Ident, Span::new(0, 0, "a")),
            (TokenKind::Plus, Span::new(0, 2, "+")),
            (TokenKind::Ident, Span::new(0, 4, "b")),
            (TokenKind::Asterisk, Span::new(0, 6, "*")),
            (TokenKind::Ident, Span::new(0, 8, "c")),
            (TokenKind::Slash, Span::new(0, 10, "/")),
            (TokenKind::Integer, Span::new(0, 12, "2")),
            (TokenKind::Minus, Span::new(0, 14, "-")),
            (TokenKind::Integer, Span::new(0, 16, "1")),
        ],
        Expr::Operator(Operator::Subtract, Span::new(0, 14, "-"), Box::new((
            Expr::Operator(
                Operator::Add,
                Span::new(0, 2, "+"),
                Box::new((
                    Expr::Ident(Span::new(0, 0, "a")),
                    Expr::Operator(
                        Operator::Divide,
                        Span::new(0, 10, "/"),
                        Box::new((
                            Expr::Operator(
                                Operator::Multiply,
                                Span::new(0, 6, "*"),
                                Box::new((
                                    Expr::Ident(Span::new(0, 4, "b")),
                                    Expr::Ident(Span::new(0, 8, "c")),
                                )),
                            ),
                            Expr::Integer(Span::new(0, 12, "2")),
                        )),
                    ),
                )),
            ),
            Expr::Integer(Span::new(0, 16, "1")),
        ))),
    }

    example! {
        parenthesized_ident,
        Expr,
        [
            (TokenKind::OpenParen, Span::new(0, 0, "(")),
            (TokenKind::Ident, Span::new(0, 1, "someValue")),
            (TokenKind::CloseParen, Span::new(0, 10, ")")),
        ],
        Expr::Ident(Span::new(0, 1, "someValue")),
    }

    example! {
        parenthesized_operators,
        Expr,
        [
            (TokenKind::OpenParen, Span::new(0, 0, "(")),
            (TokenKind::Integer, Span::new(0, 1, "1")),
            (TokenKind::Plus, Span::new(0, 3, "+")),
            (TokenKind::Integer, Span::new(0, 5, "2")),
            (TokenKind::CloseParen, Span::new(0, 6, ")")),
            (TokenKind::Asterisk, Span::new(0, 8, "*")),
            (TokenKind::Integer, Span::new(0, 10, "3")),
        ],
        Expr::Operator(Operator::Multiply, Span::new(0, 8, "*"), Box::new((
            Expr::Operator(Operator::Add, Span::new(0, 3, "+"), Box::new((
                Expr::Integer(Span::new(0, 1, "1")),
                Expr::Integer(Span::new(0, 5, "2")),
            ))),
            Expr::Integer(Span::new(0, 10, "3")),
        )))
    }

    example! {
        application,
        Expr,
        [
            (TokenKind::Ident, Span::new(0, 0, "twice")),
            (TokenKind::Ident, Span::new(0, 6, "square")),
            (TokenKind::Integer, Span::new(0, 13, "10")),
        ],
        Expr::Apply(Box::new((
            Expr::Apply(Box::new((
                Expr::Ident(Span::new(0, 0, "twice")),
                Expr::Ident(Span::new(0, 6, "square")),
            ))),
            Expr::Integer(Span::new(0, 13, "10"))),
        )),
    }

    example! {
        parenthesized_argument,
        Expr,
        [
            (TokenKind::Ident, Span::new(0, 12, "f")),
            (TokenKind::OpenParen, Span::new(0, 14, "(")),
            (TokenKind::Ident, Span::new(0, 15, "f")),
            (TokenKind::Ident, Span::new(0, 17, "x")),
            (TokenKind::CloseParen, Span::new(0, 18, ")")),
        ],
        Expr::Apply(Box::new((
            Expr::Ident(Span::new(0, 12, "f")),
            Expr::Apply(Box::new((
                Expr::Ident(Span::new(0, 15, "f")),
                Expr::Ident(Span::new(0, 17, "x")),
            ))),
        ))),
    }

    example! {
        minimal_assignment,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "value")),
            (TokenKind::Equals, Span::new(0, 6, "=")),
            (TokenKind::Integer, Span::new(0, 8, "13")),
            (TokenKind::Semicolon, Span::new(0, 10, ";")),
        ],
        Assignment::new(
            Span::new(0, 0, "value"),
            Vec::new(),
            Expr::Integer(Span::new(0, 8, "13"))
        ),
    }

    example! {
        twice_function,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "twice")),
            (TokenKind::Ident, Span::new(0, 6, "f")),
            (TokenKind::Ident, Span::new(0, 8, "x")),
            (TokenKind::Equals, Span::new(0, 10, "=")),
            (TokenKind::Ident, Span::new(0, 12, "f")),
            (TokenKind::OpenParen, Span::new(0, 14, "(")),
            (TokenKind::Ident, Span::new(0, 15, "f")),
            (TokenKind::Ident, Span::new(0, 17, "x")),
            (TokenKind::CloseParen, Span::new(0, 18, ")")),
            (TokenKind::Semicolon, Span::new(0, 19, ";")),
        ],
        Assignment::new(
            Span::new(0, 0, "twice"),
            vec![
                Span::new(0, 6, "f"),
                Span::new(0, 8, "x"),
            ],
            Expr::Apply(Box::new((
                Expr::Ident(Span::new(0, 12, "f")),
                Expr::Apply(Box::new((
                    Expr::Ident(Span::new(0, 15, "f")),
                    Expr::Ident(Span::new(0, 17, "x")),
                ))),
            ))),
        ),
    }

    example! {
        empty_sequence,
        AssignmentSeq,
        [],
        AssignmentSeq::new(Vec::new()),
    }

    example! {
        single_assignment,
        AssignmentSeq,
        [
            (TokenKind::Ident, Span::new(0, 0, "value")),
            (TokenKind::Equals, Span::new(0, 6, "=")),
            (TokenKind::Integer, Span::new(0, 8, "13")),
            (TokenKind::Semicolon, Span::new(0, 10, ";")),
        ],
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "value"),
                Vec::new(),
                Expr::Integer(Span::new(0, 8, "13")),
            ),
        ]),
    }

    example! {
        multiple_assignments,
        AssignmentSeq,
        [
            (TokenKind::Ident, Span::new(0, 0, "square")),
            (TokenKind::Ident, Span::new(0, 7, "n")),
            (TokenKind::Equals, Span::new(0, 9, "=")),
            (TokenKind::Ident, Span::new(0, 11, "n")),
            (TokenKind::Asterisk, Span::new(0, 13, "*")),
            (TokenKind::Ident, Span::new(0, 15, "n")),
            (TokenKind::Semicolon, Span::new(0, 16, ";")),
            (TokenKind::Ident, Span::new(1, 0, "twice")),
            (TokenKind::Ident, Span::new(1, 6, "f")),
            (TokenKind::Ident, Span::new(1, 8, "x")),
            (TokenKind::Equals, Span::new(1, 10, "=")),
            (TokenKind::Ident, Span::new(1, 12, "f")),
            (TokenKind::OpenParen, Span::new(1, 14, "(")),
            (TokenKind::Ident, Span::new(1, 15, "f")),
            (TokenKind::Ident, Span::new(1, 17, "x")),
            (TokenKind::CloseParen, Span::new(1, 18, ")")),
            (TokenKind::Semicolon, Span::new(1, 19, ";")),
            (TokenKind::Ident, Span::new(2, 0, "main")),
            (TokenKind::Equals, Span::new(2, 5, "=")),
            (TokenKind::Ident, Span::new(2, 7, "twice")),
            (TokenKind::Ident, Span::new(2, 13, "square")),
            (TokenKind::OpenParen, Span::new(2, 20, "(")),
            (TokenKind::Integer, Span::new(2, 21, "5")),
            (TokenKind::Minus, Span::new(2, 23, "-")),
            (TokenKind::Integer, Span::new(2, 25, "10")),
            (TokenKind::CloseParen, Span::new(2, 26, ")")),
            (TokenKind::Semicolon, Span::new(2, 27, ";")),
                    ],
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "square"),
                vec![
                    Span::new(0, 7, "n"),
                ],
                Expr::Operator(Operator::Multiply, Span::new(0, 13, "*"), Box::new((
                    Expr::Ident(Span::new(0, 11, "n")),
                    Expr::Ident(Span::new(0, 15, "n")),
                ))),
            ),
            Assignment::new(
                Span::new(1, 0, "twice"),
                vec![
                    Span::new(1, 6, "f"),
                    Span::new(1, 8, "x"),
                ],
                Expr::Apply(Box::new((
                    Expr::Ident(Span::new(1, 12, "f")),
                    Expr::Apply(Box::new((
                        Expr::Ident(Span::new(1, 15, "f")),
                        Expr::Ident(Span::new(1, 17, "x")),
                    )))
                )))
            ),
            Assignment::new(
                Span::new(2, 0, "main"),
                Vec::new(),
                Expr::Apply(Box::new((
                    Expr::Apply(Box::new((
                        Expr::Ident(Span::new(2, 7, "twice")),
                        Expr::Ident(Span::new(2, 13, "square")),
                    ))),
                    Expr::Operator(Operator::Subtract, Span::new(2, 23, "-"), Box::new((
                        Expr::Integer(Span::new(2, 21, "5")),
                        Expr::Integer(Span::new(2, 25, "10")),
                    ))),
                ))),
            ),
        ]),
    }

    error_example! {
        bad_expression_start,
        Expr,
        [
            (TokenKind::Slash, Span::new(0, 0, "/")),
        ],
        [Error::UnknownExprStart(Span::new(0, 0, "/"))],
    }

    error_example! {
        eof_in_expression,
        Expr,
        [],
        [Error::EofExpr],
    }

    error_example! {
        eof_at_assignment_start,
        Assignment,
        [],
        [Error::EofAssignment],
    }

    error_example! {
        bad_assignment_name,
        Assignment,
        [
            (TokenKind::Integer, Span::new(0, 0, "4")),
            (TokenKind::Equals, Span::new(0, 2, "=")),
            (TokenKind::Integer, Span::new(0, 4, "13")),
            (TokenKind::Semicolon, Span::new(0, 6, ";")),
        ],
        [Error::BadAssignmentName(Span::new(0, 0, "4"))],
    }

    error_example! {
        missing_assignment_name,
        Assignment,
        [
            (TokenKind::Equals, Span::new(0, 0, "=")),
            (TokenKind::Integer, Span::new(0, 2, "13")),
            (TokenKind::Semicolon, Span::new(0, 4, ";")),
        ],
        [Error::MissingAssignmentName(Span::new(0, 0, "="))],
    }

    error_example! {
        bad_assignment_arguments,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "function")),
            (TokenKind::Unknown, Span::new(0, 9, "@")),
            (TokenKind::Equals, Span::new(0, 11, "=")),
            (TokenKind::Integer, Span::new(0, 13, "13")),
            (TokenKind::Semicolon, Span::new(0, 15, ";")),
        ],
        [Error::UnknownAssignmentArgument(Span::new(0, 9, "@"))],
    }

    error_example! {
        multiple_bad_assignment_arguments,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "function")),
            (TokenKind::Unknown, Span::new(0, 9, "@")),
            (TokenKind::Integer, Span::new(0, 11, "5")),
            (TokenKind::Equals, Span::new(0, 13, "=")),
            (TokenKind::Integer, Span::new(0, 15, "13")),
            (TokenKind::Semicolon, Span::new(0, 17, ";")),
        ],
        [
            Error::UnknownAssignmentArgument(Span::new(0, 9, "@")),
            Error::UnknownAssignmentArgument(Span::new(0, 11, "5")),
        ],
    }

    error_example! {
        missing_assignment_equals,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "function")),
        ],
        [Error::EofAssignmentEquals],
    }

    error_example! {
        bad_assignment_body,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "f")),
            (TokenKind::Equals, Span::new(0, 2, "=")),
            (TokenKind::Slash, Span::new(0, 4, "/")),
            (TokenKind::Semicolon, Span::new(0, 5, ";")),
        ],
        [Error::UnknownExprStart(Span::new(0, 4, "/"))],
    }

    error_example! {
        bad_assignment_body_and_args,
        Assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "f")),
            (TokenKind::Unknown, Span::new(0, 2, "@")),
            (TokenKind::Equals, Span::new(0, 4, "=")),
            (TokenKind::Unknown, Span::new(0, 6, "%")),
            (TokenKind::Semicolon, Span::new(0, 7, ";")),
        ],
        [
            Error::UnknownAssignmentArgument(Span::new(0, 2, "@")),
            Error::UnknownExprStart(Span::new(0, 6, "%")),
        ],
    }

    error_example! {
        single_bad_assignment,
        AssignmentSeq,
        [
            (TokenKind::Integer, Span::new(0, 0, "4")),
            (TokenKind::Equals, Span::new(0, 2, "=")),
            (TokenKind::Integer, Span::new(0, 4, "13")),
            (TokenKind::Semicolon, Span::new(0, 6, ";")),
        ],
        [Error::BadAssignmentName(Span::new(0, 0, "4"))],
    }

    error_example! {
        multiple_bad_assignments,
        AssignmentSeq,
        [
            (TokenKind::Integer, Span::new(0, 0, "4")),
            (TokenKind::Equals, Span::new(0, 2, "=")),
            (TokenKind::Integer, Span::new(0, 4, "13")),
            (TokenKind::Semicolon, Span::new(0, 6, ";")),
            (TokenKind::Integer, Span::new(1, 0, "4")),
            (TokenKind::Equals, Span::new(1, 2, "=")),
            (TokenKind::Integer, Span::new(1, 4, "13")),
            (TokenKind::Semicolon, Span::new(1, 6, ";")),
        ],
        [
            Error::BadAssignmentName(Span::new(0, 0, "4")),
            Error::BadAssignmentName(Span::new(1, 0, "4")),
        ],
    }

    error_example! {
        bad_assignments_with_eof,
        AssignmentSeq,
        [
            (TokenKind::Integer, Span::new(0, 0, "4")),
            (TokenKind::Equals, Span::new(0, 2, "=")),
            (TokenKind::Integer, Span::new(0, 4, "13")),
            (TokenKind::Semicolon, Span::new(0, 6, ";")),
            (TokenKind::Integer, Span::new(1, 0, "4")),
        ],
        [
            Error::BadAssignmentName(Span::new(0, 0, "4")),
            Error::BadAssignmentName(Span::new(1, 0, "4")),
        ],
    }

    example! {
        accept_no_terminating_semicolon,
        AssignmentSeq,
        [
            (TokenKind::Ident, Span::new(0, 0, "main")),
            (TokenKind::Equals, Span::new(0, 5, "=")),
            (TokenKind::Integer, Span::new(0, 7, "0")),
        ],
        AssignmentSeq::new(vec![
            Assignment::new(
                Span::new(0, 0, "main"),
                Vec::new(),
                Expr::Integer(Span::new(0, 7, "0")),
            ),
        ])
    }
}
