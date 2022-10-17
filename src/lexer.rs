use std::str::Chars;

pub type Token<'a> = (TokenKind, TokenInfo<'a>);

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

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }

    pub fn source(&self) -> &str {
        self.source
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

pub fn lex(input: &str) -> impl Iterator<Item = Token> + '_ {
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
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
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
            '+' => Some((TokenKind::Plus, TokenInfo::new(line, column, "+"))),
            '-' => Some((TokenKind::Minus, TokenInfo::new(line, column, "-"))),
            '*' => Some((TokenKind::Asterisk, TokenInfo::new(line, column, "*"))),
            '/' => Some((TokenKind::Slash, TokenInfo::new(line, column, "/"))),
            ';' => Some((TokenKind::Semicolon, TokenInfo::new(line, column, ";"))),
            '=' => Some((TokenKind::Equals, TokenInfo::new(line, column, "="))),
            '(' => Some((TokenKind::OpenParen, TokenInfo::new(line, column, "("))),
            ')' => Some((TokenKind::CloseParen, TokenInfo::new(line, column, ")"))),
            c if c.is_whitespace() => self.next(),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{lex, Token, TokenInfo, TokenKind};

    #[test]
    fn can_lex_empty_string() {
        let output = lex("");
        assert_eq!(0, output.count());
    }

    #[test]
    fn can_lex_single_character_tokens() {
        const EXPECTED: &[Token] = &[
            (TokenKind::Plus, TokenInfo::new(0, 0, "+")),
            (TokenKind::Minus, TokenInfo::new(0, 1, "-")),
            (TokenKind::Asterisk, TokenInfo::new(0, 2, "*")),
            (TokenKind::Slash, TokenInfo::new(0, 3, "/")),
            (TokenKind::Semicolon, TokenInfo::new(0, 4, ";")),
            (TokenKind::Equals, TokenInfo::new(0, 5, "=")),
            (TokenKind::OpenParen, TokenInfo::new(0, 6, "(")),
            (TokenKind::CloseParen, TokenInfo::new(0, 7, ")")),
        ];
        let output: Vec<_> = lex("+-*/;=()").collect();
        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn whitespace_is_ignored() {
        const EXPECTED: &[Token] = &[
            (TokenKind::OpenParen, TokenInfo::new(0, 1, "(")),
            (TokenKind::CloseParen, TokenInfo::new(0, 7, ")")),
        ];
        let output: Vec<_> = lex("\t(     )\n").collect();
        assert_eq!(EXPECTED, output);
    }

    #[test]
    fn can_get_line_information() {
        const EXPECTED: &[Token] = &[
            (TokenKind::Plus, TokenInfo::new(0, 0, "+")),
            (TokenKind::Asterisk, TokenInfo::new(1, 0, "*")),
            (TokenKind::Minus, TokenInfo::new(3, 3, "-")),
        ];
        let output: Vec<_> = lex("+\n*\n\n   -\n").collect();
        assert_eq!(EXPECTED, output);
    }
}
