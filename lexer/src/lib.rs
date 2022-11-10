#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! The lexer for a programming language. Recognizes `+`, `-`, `*`, `/`, `;`, `=`, `(`, `)`,
//! integers `/[0-9]+/` and identifiers `/[a-zA-Z_][a-zA-Z0-9_]*/`.

use language::{TokenInfo, TokenKind};
use std::{iter::Peekable, str::CharIndices};

/// The type produced by the `lex` iterator. Contains either a token kind or a lex error,
/// alongside position information.
pub type LexResult<'a> = (Result<TokenKind, Error>, TokenInfo<'a>);

/// Return an iterator over the tokens of `input`.
pub fn lex(input: &str) -> impl Iterator<Item = LexResult> + '_ {
    Lexer::new(input)
}

struct Lexer<'a> {
    string: &'a str,
    chars: Peekable<CharIndices<'a>>,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            chars: string.char_indices().peekable(),
            line: 0,
            column: 0,
        }
    }

    fn integer(&mut self, line: u32, column: u32, start: usize) -> Option<LexResult<'a>> {
        match self.chars.peek() {
            Some(&(_, c)) if c.is_ascii_digit() => {
                self.chars.next();
                self.column += 1;
                self.integer(line, column, start)
            }
            Some(&(index, _)) => {
                let info = TokenInfo::new(line, column, &self.string[start..index]);
                Some((Ok(TokenKind::Integer), info))
            }
            None => {
                let info = TokenInfo::new(line, column, &self.string[start..]);
                Some((Ok(TokenKind::Integer), info))
            }
        }
    }

    fn ident(&mut self, line: u32, column: u32, start: usize) -> Option<LexResult<'a>> {
        match self.chars.peek() {
            Some(&(_, c)) if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_' => {
                self.chars.next();
                self.column += 1;
                self.ident(line, column, start)
            }
            Some(&(index, _)) => {
                let info = TokenInfo::new(line, column, &self.string[start..index]);
                Some((Ok(TokenKind::Ident), info))
            }
            None => {
                let info = TokenInfo::new(line, column, &self.string[start..]);
                Some((Ok(TokenKind::Ident), info))
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<LexResult<'a>> {
        let (index, c) = self.chars.next()?;

        let line = self.line;
        let column = self.column;
        // TODO: Handle non-byte characters here.
        let string = &self.string[index..index + 1];
        let info = TokenInfo::new(line, column, string);
        self.column += 1;

        match c {
            '\n' => {
                self.line += 1;
                self.column = 0;
                self.next()
            }
            '+' => Some((Ok(TokenKind::Plus), info)),
            '-' => Some((Ok(TokenKind::Minus), info)),
            '*' => Some((Ok(TokenKind::Asterisk), info)),
            '/' => Some((Ok(TokenKind::Slash), info)),
            ';' => Some((Ok(TokenKind::Semicolon), info)),
            '=' => Some((Ok(TokenKind::Equals), info)),
            '(' => Some((Ok(TokenKind::OpenParen), info)),
            ')' => Some((Ok(TokenKind::CloseParen), info)),
            d if d.is_ascii_digit() => self.integer(line, column, index),
            a if a.is_ascii_alphabetic() || a == '_' => self.ident(line, column, index),
            c if c.is_whitespace() => self.next(),
            _ => Some((Err(Error::UnknownCharacter), info)),
        }
    }
}

/// A lexical error.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Error {
    /// An unknown character was found in the input string.
    UnknownCharacter,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Error::UnknownCharacter => "unknown character",
        };
        write!(f, "{string}")
    }
}

impl std::error::Error for Error {}
