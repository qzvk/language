#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Lexical analysis crate. Provides the `lex` function, which converts a string into a stream of
//! tokens.

use std::{iter::Peekable, str::CharIndices};

/// A kind of token. The syntactic function of a token.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// The ';'' character
    Semicolon,

    /// The '=' character
    Equals,

    /// The '(' character
    OpenParen,

    /// The ')' character
    CloseParen,

    /// The '+' character
    Plus,

    /// The '-' character
    Minus,

    /// The '*' character
    Asterisk,

    /// The '/' character
    Slash,

    /// An identifier (an ASCII letter or underscore, followed by zero or more underscores and
    /// ASCII alphanumeric characters)
    Ident,

    /// An integer (one or more digits)
    Integer,

    /// An unkown token.
    Unknown,
}

/// A span of the input string.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span<'a> {
    line: u32,
    column: u32,
    source: &'a str,
}

impl<'a> Span<'a> {
    /// Create a new span with the given `line`, `column` and `source`.
    pub const fn new(line: u32, column: u32, source: &'a str) -> Self {
        Self {
            line,
            column,
            source,
        }
    }
}

/// An iterator over the tokens of an input string.
pub fn lex(input: &str) -> impl Iterator<Item = (TokenKind, Span)> + '_ {
    Tokens::new(input)
}

struct Tokens<'a> {
    input: &'a str,
    line: u32,
    column: u32,
    chars_indices: Peekable<CharIndices<'a>>,
}

impl<'a> Tokens<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            line: 0,
            column: 0,
            chars_indices: input.char_indices().peekable(),
        }
    }

    fn next_ident(&mut self, column: u32, start: usize) -> Option<(TokenKind, Span<'a>)> {
        match self.chars_indices.peek() {
            None => Some((
                TokenKind::Ident,
                Span::new(self.line, column, &self.input[start..]),
            )),

            Some(&(_, c)) if c.is_ascii_alphanumeric() || c == '_' => {
                self.chars_indices.next();
                self.column += 1;
                self.next_ident(column, start)
            }

            Some(&(end, _)) => Some((
                TokenKind::Ident,
                Span::new(self.line, column, &self.input[start..end]),
            )),
        }
    }

    fn next_integer(&mut self, column: u32, start: usize) -> Option<(TokenKind, Span<'a>)> {
        match self.chars_indices.peek() {
            None => Some((
                TokenKind::Integer,
                Span::new(self.line, column, &self.input[start..]),
            )),

            Some(&(_, c)) if c.is_ascii_digit() => {
                self.chars_indices.next();
                self.column += 1;
                self.next_integer(column, start)
            }

            Some(&(end, _)) => Some((
                TokenKind::Integer,
                Span::new(self.line, column, &self.input[start..end]),
            )),
        }
    }

    fn next_unknown(&mut self, column: u32, start: usize) -> Option<(TokenKind, Span<'a>)> {
        match self.chars_indices.peek() {
            None => Some((
                TokenKind::Unknown,
                Span::new(self.line, column, &self.input[start..]),
            )),

            Some(&(end, c)) if is_known(c) => Some((
                TokenKind::Unknown,
                Span::new(self.line, column, &self.input[start..end]),
            )),

            Some(_) => {
                self.chars_indices.next();
                self.column += 1;
                self.next_unknown(column, start)
            }
        }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = (TokenKind, Span<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        let (index, c) = self.chars_indices.next()?;
        let column = self.column;
        self.column += 1;

        match c {
            ';' => Some((TokenKind::Semicolon, Span::new(self.line, column, ";"))),
            '=' => Some((TokenKind::Equals, Span::new(self.line, column, "="))),
            '(' => Some((TokenKind::OpenParen, Span::new(self.line, column, "("))),
            ')' => Some((TokenKind::CloseParen, Span::new(self.line, column, ")"))),
            '+' => Some((TokenKind::Plus, Span::new(self.line, column, "+"))),
            '-' => Some((TokenKind::Minus, Span::new(self.line, column, "-"))),
            '*' => Some((TokenKind::Asterisk, Span::new(self.line, column, "*"))),
            '/' => Some((TokenKind::Slash, Span::new(self.line, column, "/"))),
            '\n' => {
                self.line += 1;
                self.column = 0;
                self.next()
            }
            c if c.is_ascii_alphabetic() || c == '_' => self.next_ident(column, index),
            c if c.is_ascii_digit() || c == '_' => self.next_integer(column, index),
            c if c.is_ascii_whitespace() => self.next(),

            c if !is_known(c) => self.next_unknown(column, index),
            _ => unreachable!(),
        }
    }
}

const fn is_known(c: char) -> bool {
    match c {
        ';' | '=' | '(' | ')' | '+' | '-' | '*' | '/' | '_' => true,
        c if c.is_ascii_alphanumeric() || c.is_ascii_whitespace() => true,
        _ => false,
    }
}
