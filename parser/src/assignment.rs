use crate::expr::{self, Expr};
use lexer::{Span, TokenKind};
use std::iter::Peekable;

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
}

pub fn parse<'a, I>(tokens: &mut Peekable<I>) -> Result<Assignment<'a>, Error>
where
    I: Iterator<Item = (TokenKind, Span<'a>)>,
{
    let name = match tokens.next() {
        Some((TokenKind::Ident, span)) => span,
        t => todo!("handle {t:?}"),
    };

    let mut args = Vec::new();

    loop {
        match tokens.next() {
            Some((TokenKind::Equals, _)) => break,
            Some((TokenKind::Ident, span)) => args.push(span),
            t => todo!("handle {t:?}"),
        }
    }

    let body = expr::parse(tokens).unwrap_or_else(|e| todo!("handle error in body {e:?}"));

    Ok(Assignment::new(name, args, body))
}

#[derive(Debug)]
pub enum Error {}

#[cfg(test)]
mod tests {
    use super::{Assignment, Expr};
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
        minimal_assignment,
        [
            (TokenKind::Ident, Span::new(0, 0, "value")),
            (TokenKind::Equals, Span::new(0, 6, "=")),
            (TokenKind::Integer, Span::new(0, 8, "13")),
        ],
        Assignment::new(
            Span::new(0, 0, "value"),
            Vec::new(),
            Expr::Integer(Span::new(0, 8, "13"))
        ),
    }

    example! {
        twice_function,
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
}
