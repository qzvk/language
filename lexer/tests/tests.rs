use language::{TokenInfo, TokenKind};
use lexer::{lex, Error, LexResult};

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
    assert_eq!("(integer)", TokenKind::Integer.to_string());
    assert_eq!("(ident)", TokenKind::Ident.to_string());
}

#[test]
fn can_display_token_info() {
    assert_eq!("1:1 \"\"", TokenInfo::new(0, 0, "").to_string());
    assert_eq!("14:5 \"hello\"", TokenInfo::new(13, 4, "hello").to_string());
}

#[test]
fn unknown_characters_are_reported() {
    const EXPECTED: &[LexResult] = &[
        (Err(Error::UnknownCharacter), TokenInfo::new(0, 0, "%")),
        (Err(Error::UnknownCharacter), TokenInfo::new(0, 1, "@")),
        (Ok(TokenKind::Plus), TokenInfo::new(1, 0, "+")),
        (Err(Error::UnknownCharacter), TokenInfo::new(1, 2, "$")),
    ];
    let actual: Vec<_> = lex("%@\n+ $\n").collect();
    assert_eq!(EXPECTED, actual);
}

#[test]
fn can_display_error() {
    assert_eq!("unknown character", Error::UnknownCharacter.to_string());
}

#[test]
fn can_lex_integers() {
    const SOURCE: &str = "0 12 345 6789\n 01234 567890";
    const EXPECTED: &[LexResult] = &[
        (Ok(TokenKind::Integer), TokenInfo::new(0, 0, "0")),
        (Ok(TokenKind::Integer), TokenInfo::new(0, 2, "12")),
        (Ok(TokenKind::Integer), TokenInfo::new(0, 5, "345")),
        (Ok(TokenKind::Integer), TokenInfo::new(0, 9, "6789")),
        (Ok(TokenKind::Integer), TokenInfo::new(1, 1, "01234")),
        (Ok(TokenKind::Integer), TokenInfo::new(1, 7, "567890")),
    ];
    let actual: Vec<_> = lex(SOURCE).collect();
    assert_eq!(EXPECTED, actual);
}

#[test]
fn can_lex_idents() {
    const SOURCE: &str = "a helloEXAMPLE\n_5x\n wx_yz";
    const EXPECTED: &[LexResult] = &[
        (Ok(TokenKind::Ident), TokenInfo::new(0, 0, "a")),
        (Ok(TokenKind::Ident), TokenInfo::new(0, 2, "helloEXAMPLE")),
        (Ok(TokenKind::Ident), TokenInfo::new(1, 0, "_5x")),
        (Ok(TokenKind::Ident), TokenInfo::new(2, 1, "wx_yz")),
    ];
    let actual: Vec<_> = lex(SOURCE).collect();
    assert_eq!(EXPECTED, actual);
}

#[test]
fn can_lex_short_example() {
    const SOURCE: &str =
        "applyTwice f x = f (f x);\nsquare n = n * n;\napplyTwice square (4 - 11)\n";
    const EXPECTED: &[LexResult] = &[
        (Ok(TokenKind::Ident), TokenInfo::new(0, 0, "applyTwice")),
        (Ok(TokenKind::Ident), TokenInfo::new(0, 11, "f")),
        (Ok(TokenKind::Ident), TokenInfo::new(0, 13, "x")),
        (Ok(TokenKind::Equals), TokenInfo::new(0, 15, "=")),
        (Ok(TokenKind::Ident), TokenInfo::new(0, 17, "f")),
        (Ok(TokenKind::OpenParen), TokenInfo::new(0, 19, "(")),
        (Ok(TokenKind::Ident), TokenInfo::new(0, 20, "f")),
        (Ok(TokenKind::Ident), TokenInfo::new(0, 22, "x")),
        (Ok(TokenKind::CloseParen), TokenInfo::new(0, 23, ")")),
        (Ok(TokenKind::Semicolon), TokenInfo::new(0, 24, ";")),
        (Ok(TokenKind::Ident), TokenInfo::new(1, 0, "square")),
        (Ok(TokenKind::Ident), TokenInfo::new(1, 7, "n")),
        (Ok(TokenKind::Equals), TokenInfo::new(1, 9, "=")),
        (Ok(TokenKind::Ident), TokenInfo::new(1, 11, "n")),
        (Ok(TokenKind::Asterisk), TokenInfo::new(1, 13, "*")),
        (Ok(TokenKind::Ident), TokenInfo::new(1, 15, "n")),
        (Ok(TokenKind::Semicolon), TokenInfo::new(1, 16, ";")),
        (Ok(TokenKind::Ident), TokenInfo::new(2, 0, "applyTwice")),
        (Ok(TokenKind::Ident), TokenInfo::new(2, 11, "square")),
        (Ok(TokenKind::OpenParen), TokenInfo::new(2, 18, "(")),
        (Ok(TokenKind::Integer), TokenInfo::new(2, 19, "4")),
        (Ok(TokenKind::Minus), TokenInfo::new(2, 21, "-")),
        (Ok(TokenKind::Integer), TokenInfo::new(2, 23, "11")),
        (Ok(TokenKind::CloseParen), TokenInfo::new(2, 25, ")")),
    ];
    let actual: Vec<_> = lex(SOURCE).collect();
    assert_eq!(EXPECTED, actual);
}
