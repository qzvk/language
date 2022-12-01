use lexer::{Span, TokenKind};

macro_rules! example {
    ($name:ident : $from:literal => [ $($to:expr),* $(,)? ]) => {
        #[test]
        fn $name() {
            const INPUT: &str = $from;
            const EXPECTED: &[(::lexer::TokenKind, ::lexer::Span)] = &[
                $(
                    $to,
                )*
            ];
            let actual: ::std::vec::Vec<_> = ::lexer::lex(INPUT).collect();
            assert_eq!(EXPECTED, actual.as_slice(), "Input was {INPUT:?}");
        }
    };
}

example! {
    empty: "" => []
}

example! {
    single_characters: ";=()+-*/" => [
        (TokenKind::Semicolon, Span::new(0, 0, ";")),
        (TokenKind::Equals, Span::new(0, 1, "=")),
        (TokenKind::OpenParen, Span::new(0, 2, "(")),
        (TokenKind::CloseParen, Span::new(0, 3, ")")),
        (TokenKind::Plus, Span::new(0, 4, "+")),
        (TokenKind::Minus, Span::new(0, 5, "-")),
        (TokenKind::Asterisk, Span::new(0, 6, "*")),
        (TokenKind::Slash, Span::new(0, 7, "/")),
    ]
}

example! {
    idents: "a BCD efg_hij _klm NO_P_Q _10x" => [
        (TokenKind::Ident, Span::new(0, 0, "a")),
        (TokenKind::Ident, Span::new(0, 2, "BCD")),
        (TokenKind::Ident, Span::new(0, 6, "efg_hij")),
        (TokenKind::Ident, Span::new(0, 14, "_klm")),
        (TokenKind::Ident, Span::new(0, 19, "NO_P_Q")),
        (TokenKind::Ident, Span::new(0, 26, "_10x")),
    ]
}

example! {
    integers: "0 123456789 99999999999999999999999999" => [
        (TokenKind::Integer, Span::new(0, 0, "0")),
        (TokenKind::Integer, Span::new(0, 2, "123456789")),
        (TokenKind::Integer, Span::new(0, 12, "99999999999999999999999999")),
    ]
}

example! {
    newlines: "first\n second\n  third\n" => [
        (TokenKind::Ident, Span::new(0, 0, "first")),
        (TokenKind::Ident, Span::new(1, 1, "second")),
        (TokenKind::Ident, Span::new(2, 2, "third")),
    ]
}

example! {
    small_example: "square n = n * n;\ntwice f x = f (f x);\nsquare twice (5 - 10)" => [
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
        (TokenKind::Ident, Span::new(2, 0, "square")),
        (TokenKind::Ident, Span::new(2, 7, "twice")),
        (TokenKind::OpenParen, Span::new(2, 13, "(")),
        (TokenKind::Integer, Span::new(2, 14, "5")),
        (TokenKind::Minus, Span::new(2, 16, "-")),
        (TokenKind::Integer, Span::new(2, 18, "10")),
        (TokenKind::CloseParen, Span::new(2, 20, ")")),
    ]
}

example! {
    unknown: "£ % \\@#? hello! 100:00" => [
        (TokenKind::Unknown, Span::new(0, 0, "£")),
        (TokenKind::Unknown, Span::new(0, 2, "%")),
        (TokenKind::Unknown, Span::new(0, 4, "\\@#?")),
        (TokenKind::Ident, Span::new(0, 9, "hello")),
        (TokenKind::Unknown, Span::new(0, 14, "!")),
        (TokenKind::Integer, Span::new(0, 16, "100")),
        (TokenKind::Unknown, Span::new(0, 19, ":")),
        (TokenKind::Integer, Span::new(0, 20, "00")),
    ]
}
