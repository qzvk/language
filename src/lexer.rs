use std::str::Chars;

pub type LexResult<'a> = (Result<TokenKind, Error>, TokenInfo<'a>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenInfo<'a> {
    line: u32,
    column: u32,
    source: &'a str,
}

impl<'a> TokenInfo<'a> {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Semicolon,
    Equals,
    OpenParen,
    CloseParen,
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
        };
        write!(f, "({string})")
    }
}

pub fn lex(input: &str) -> impl Iterator<Item = LexResult> + '_ {
    Lexer::new(input)
}

struct Lexer<'a> {
    input: Chars<'a>,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars(),
            line: 0,
            column: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<LexResult<'a>> {
        let c = self.input.next()?;

        let line = self.line;
        let column = self.column;
        self.column += 1;

        match c {
            '\n' => {
                self.line += 1;
                self.column = 0;
                self.next()
            }
            '+' => Some((Ok(TokenKind::Plus), TokenInfo::new(line, column, "+"))),
            '-' => Some((Ok(TokenKind::Minus), TokenInfo::new(line, column, "-"))),
            '*' => Some((Ok(TokenKind::Asterisk), TokenInfo::new(line, column, "*"))),
            '/' => Some((Ok(TokenKind::Slash), TokenInfo::new(line, column, "/"))),
            ';' => Some((Ok(TokenKind::Semicolon), TokenInfo::new(line, column, ";"))),
            '=' => Some((Ok(TokenKind::Equals), TokenInfo::new(line, column, "="))),
            '(' => Some((Ok(TokenKind::OpenParen), TokenInfo::new(line, column, "("))),
            ')' => Some((Ok(TokenKind::CloseParen), TokenInfo::new(line, column, ")"))),
            c if c.is_whitespace() => self.next(),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Error {}

#[cfg(test)]
mod tests {
    use super::{lex, LexResult, TokenInfo, TokenKind};

    #[test]
    fn can_lex_empty_string() {
        let output = lex("");
        assert_eq!(0, output.count());
    }

    #[test]
    fn can_lex_single_character_tokens() {
        const EXPECTED: &[LexResult] = &[
            (Ok(TokenKind::Plus), TokenInfo::new(0, 0, "+")),
            (Ok(TokenKind::Minus), TokenInfo::new(0, 1, "-")),
            (Ok(TokenKind::Asterisk), TokenInfo::new(0, 2, "*")),
            (Ok(TokenKind::Slash), TokenInfo::new(0, 3, "/")),
            (Ok(TokenKind::Semicolon), TokenInfo::new(0, 4, ";")),
            (Ok(TokenKind::Equals), TokenInfo::new(0, 5, "=")),
            (Ok(TokenKind::OpenParen), TokenInfo::new(0, 6, "(")),
            (Ok(TokenKind::CloseParen), TokenInfo::new(0, 7, ")")),
        ];
        let output: Vec<_> = lex("+-*/;=()").collect();
        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn whitespace_is_ignored() {
        const EXPECTED: &[LexResult] = &[
            (Ok(TokenKind::OpenParen), TokenInfo::new(0, 1, "(")),
            (Ok(TokenKind::CloseParen), TokenInfo::new(0, 7, ")")),
        ];
        let output: Vec<_> = lex("\t(     )\n").collect();
        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn can_get_line_information() {
        const EXPECTED: &[LexResult] = &[
            (Ok(TokenKind::Plus), TokenInfo::new(0, 0, "+")),
            (Ok(TokenKind::Asterisk), TokenInfo::new(1, 0, "*")),
            (Ok(TokenKind::Minus), TokenInfo::new(3, 3, "-")),
        ];
        let output: Vec<_> = lex("+\n*\n\n   -\n").collect();
        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn can_display_token_kinds() {
        assert_eq!("(plus)", TokenKind::Plus.to_string());
        assert_eq!("(minus)", TokenKind::Minus.to_string());
        assert_eq!("(asterisk)", TokenKind::Asterisk.to_string());
        assert_eq!("(slash)", TokenKind::Slash.to_string());
        assert_eq!("(semicolon)", TokenKind::Semicolon.to_string());
        assert_eq!("(equals)", TokenKind::Equals.to_string());
        assert_eq!("(open-paren)", TokenKind::OpenParen.to_string());
        assert_eq!("(close-paren)", TokenKind::CloseParen.to_string());
    }

    #[test]
    fn can_display_token_info() {
        assert_eq!("1:1 \"\"", TokenInfo::new(0, 0, "").to_string());
        assert_eq!("14:5 \"hello\"", TokenInfo::new(13, 4, "hello").to_string());
    }
}
