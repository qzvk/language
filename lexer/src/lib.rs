#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! The lexer for a programming language. Recognizes `+`, `-`, `*`, `/`, `;`, `=`, `(`, `)`,
//! integers `/[0-9]+/` and identifiers `/[a-zA-Z_][a-zA-Z0-9_]*/`.

use std::{iter::Peekable, str::CharIndices};

/// The type produced by the `lex` iterator. Contains either a token kind or a lex error,
/// alongside position information.
pub type LexResult<'a> = (Result<TokenKind, Error>, TokenInfo<'a>);

/// Token (or lexical error) position information.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenInfo<'a> {
    line: u32,
    column: u32,
    source: &'a str,
}

impl<'a> TokenInfo<'a> {
    /// Create new token info, with the given `line`, `column`, and string of associated `source`.
    pub const fn new(line: u32, column: u32, source: &'a str) -> Self {
        Self {
            line,
            column,
            source,
        }
    }
}

impl<'a> std::fmt::Display for TokenInfo<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Line and column numbers are 1-indexed, since they're for humans.
        write!(f, "{}:{} {:?}", self.line + 1, self.column + 1, self.source)
    }
}

/// A kind of token producable by lexical analysis.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    /// A plus (`+`)
    Plus,

    /// A minus (`-`)
    Minus,

    /// An asterisk (`*`)
    Asterisk,

    /// A slash (`/`)
    Slash,

    /// A semicolon (`;`)
    Semicolon,

    /// An equals (`=`)
    Equals,

    /// An open parenthesis (`(`)
    OpenParen,

    /// A close parenthesis (`)`)
    CloseParen,

    /// An integer (`/[0-9+]/`)
    Integer,

    /// An identifier (`/[a-zA-Z_][a-zA-Z0-9_]*/`)
    Ident,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenKind::Plus => "plus",
            TokenKind::Minus => "minus",
            TokenKind::Asterisk => "asterisk",
            TokenKind::Slash => "slash",
            TokenKind::Semicolon => "semicolon",
            TokenKind::Equals => "equals",
            TokenKind::OpenParen => "open-paren",
            TokenKind::CloseParen => "close-paren",
            TokenKind::Integer => "integer",
            TokenKind::Ident => "ident",
        };
        write!(f, "({string})")
    }
}

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
