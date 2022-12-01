#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Lexical analysis crate. Provides the `lex` function, which converts a string into a stream of
//! tokens.

/// A kind of token. The syntactic function of a token.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {}

/// A span of the input string.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {}

/// An iterator over the tokens of an input string.
pub fn lex(input: &str) -> impl Iterator<Item = (TokenKind, Span)> + '_ {
    Tokens::new(input)
}

struct Tokens<'a> {
    _input: &'a str,
}

impl<'a> Tokens<'a> {
    fn new(input: &'a str) -> Self {
        Self { _input: input }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = (TokenKind, Span);

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
