macro_rules! example {
    ($name:ident : $from:literal => [ $($to:expr),* $(,)? ]) => {
        #[test]
        fn $name() {
            let input = $from;
            let expected: &[(::lexer::TokenKind, ::lexer::Span)] = &[
                $(
                    $to,
                )*
            ];
            let actual: Vec<_> = ::lexer::lex(input).collect();
            assert_eq!(expected, actual.as_slice(), "Input was {input}");
        }
    };
}

example! { empty : "" => [] }
