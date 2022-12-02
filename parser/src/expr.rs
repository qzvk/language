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

fn parse<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr<'a>, Error>
where
    I: Iterator<Item = (TokenKind, Span<'a>)>,
{
    parse_with_power(tokens, 0)
}

fn parse_with_power<'a, I>(
    tokens: &mut Peekable<I>,
    minimum_binding_power: u8,
) -> Result<Expr<'a>, Error>
where
    I: Iterator<Item = (TokenKind, Span<'a>)>,
{
    let mut lhs = match tokens.next() {
        Some((TokenKind::Integer, span)) => Expr::Integer(span),
        Some((TokenKind::Ident, span)) => Expr::Ident(span),
        Some((TokenKind::OpenParen, _)) => {
            let lhs = parse_with_power(tokens, 0)?;
            assert!(matches!(tokens.next(), Some((TokenKind::CloseParen, _))));
            lhs
        }
        t => todo!("handle {t:?}"),
    };

    loop {
        let (operator, span) = match tokens.peek() {
            None => break,
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
            t => todo!("handle {t:?}"),
        };

        let (left_binding_power, right_binding_power) = operator.infix_binding_powers();
        if left_binding_power < minimum_binding_power {
            break;
        }

        tokens.next();

        let rhs = parse_with_power(tokens, right_binding_power)?;
        lhs = Expr::Operator(operator, span, Box::new((lhs, rhs)));
    }

    Ok(lhs)
}

#[derive(Debug)]
pub enum Error {}

#[cfg(test)]
mod tests {
    use super::{Expr, Operator};
    use lexer::{Span, TokenKind};

    macro_rules! example {
        ($name:ident, [ $($input:expr),* $(,)? ], $expected:expr $(,)?) => {
            #[test]
            fn $name() {
                let mut input = [$($input),*]
                    .into_iter()
                    .peekable();
                let expected = $expected;
                let actual = super::parse(&mut input).unwrap();
                assert_eq!(expected, actual);
            }
        };
    }

    example! {
        integer,
        [(TokenKind::Integer, Span::new(0, 0, "12345"))],
        Expr::Integer(Span::new(0, 0, "12345")),
    }

    example! {
        identifier,
        [
            (TokenKind::Ident, Span::new(0, 1, "hello"))
        ],
        Expr::Ident(Span::new(0, 1, "hello")),
    }

    example! {
        operators,
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
        [
            (TokenKind::OpenParen, Span::new(0, 0, "(")),
            (TokenKind::Ident, Span::new(0, 1, "someValue")),
            (TokenKind::CloseParen, Span::new(0, 10, ")")),
        ],
        Expr::Ident(Span::new(0, 1, "someValue")),
    }

    example! {
        parenthesized_operators,
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
}
