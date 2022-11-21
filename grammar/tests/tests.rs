use std::collections::HashSet;

use grammar::{
    Error, Grammar, Nonterminal, ParseAction, ParseTable, ParseTableConflict, ParseTree, Symbol,
    Terminal,
};

/// The number of elements in a comma-separated ident list.
macro_rules! count_list {
    ($first:ident, $($rest:ident),+) => {
        1 + count_list!($($rest),+)
    };
    ($single:ident) => {1};
    () => {0};
}

/// Auto-implementation of Nonterminal trait.
macro_rules! impl_nonterminal {
    ($name:ident : [ $($variant:ident),+ ] ) => {
        impl crate::Nonterminal for $name {
            type Iterator = std::array::IntoIter<Self, {Self::COUNT}>;

            const COUNT: usize = count_list!($($variant),+);

            fn all() -> Self::Iterator {
                [$(Self::$variant),+].into_iter()
            }

            fn index(self) -> usize {
                self as usize
            }

            fn from_index(index: usize) -> Self {
                Self::all().nth(index).unwrap()
            }

            fn as_str(&self) -> &'static str {
                match self {
                    $(
                        Self::$variant => stringify!($variant),
                    )+
                }
            }
        }

    };
}

/// Auto-implementation of Terminal trait.
macro_rules! impl_terminal {
    ($name:ident : [ $($variant:ident),+ ] ) => {
        impl crate::Terminal for $name {
            type Iterator = std::array::IntoIter<Self, {Self::COUNT}>;

            const COUNT: usize = count_list!($($variant),+);

            fn all() -> Self::Iterator {
                [$(Self::$variant),+].into_iter()
            }

            fn index(self) -> usize {
                self as usize
            }

            fn from_index(index: usize) -> Self {
                Self::all().nth(index).unwrap()
            }

            fn as_str(&self) -> &'static str {
                match self {
                    $(
                        Self::$variant => stringify!($variant),
                    )+
                }
            }
        }

    };
}

/// Auto-define a nonterminal enum.
macro_rules! define_nonterminal {
    ($name:ident: [$($variant:ident),*] $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum $name { $($variant),* }
        impl_nonterminal! { $name: [$($variant),*] }
    };
}

/// Auto-define a terminal enum.
macro_rules! define_terminal {
    ($name:ident: [$($variant:ident),*] $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum $name { $($variant),* }
        impl_terminal! { $name: [$($variant),*] }
    };
}

#[test]
fn can_create_grammar() {
    define_nonterminal! { SimpleNonterminal: [S, A, B] };
    define_terminal! { SimpleTerminal: [C, D] };

    use {SimpleNonterminal::*, SimpleTerminal::*};

    let mut grammar = Grammar::new(S);

    assert_eq!(0, grammar.rule_count());

    grammar.add_rule(A).terminal(C);
    assert_eq!(1, grammar.rule_count());

    grammar.add_rule(B).terminal(D);
    assert_eq!(2, grammar.rule_count());

    grammar.add_rule(A).terminal(D).nonterminal(B).terminal(C);
    assert_eq!(3, grammar.rule_count());

    grammar.add_rule(S).nonterminal(A);
    assert_eq!(4, grammar.rule_count());

    let _proper_grammar = grammar.validate().unwrap();
}

#[test]
fn unproductive_grammar_is_not_proper() {
    define_nonterminal!(N: [S, X, Y]);
    define_terminal!(T: [A]);
    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::X).terminal(T::A);
    grammar.add_rule(N::X).nonterminal(N::Y);

    let error = grammar.validate().unwrap_err();
    assert_eq!(Error::UnproductiveNonterminals(vec![N::Y]), error);
}

#[test]
fn unreachable_grammar_is_not_proper() {
    define_nonterminal!(N: [S, X, Y]);
    define_terminal!(T: [A, B, C]);
    // unreachable: b, Y, c

    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::X).terminal(T::A);
    grammar.add_rule(N::Y).terminal(T::B);

    let error = grammar.validate().unwrap_err();
    assert_eq!(
        Error::UnreachableSymbols(vec![N::Y], vec![T::B, T::C]),
        error
    );
}

#[test]
fn epsilon_productions_are_not_proper() {
    define_nonterminal!(N: [S, X, Y, Z]);
    define_terminal!(T: [A, B, C]);

    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::S).nonterminal(N::Y);
    grammar.add_rule(N::S).nonterminal(N::Z);
    grammar.add_rule(N::Z);
    grammar.add_rule(N::X);
    grammar.add_rule(N::X).terminal(T::A).nonterminal(N::X);
    grammar
        .add_rule(N::Y)
        .terminal(T::B)
        .nonterminal(N::Z)
        .terminal(T::C);

    let error = grammar.validate().unwrap_err();
    assert_eq!(Error::EpsilonProductions(vec![N::X, N::Z]), error);
}

#[test]
fn cycles_are_not_proper() {
    define_nonterminal!(N: [S, U, V, W, X, Y, Z]);
    define_terminal!(T: [A, B, C]);

    let mut grammar = Grammar::new(N::S);

    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::S).nonterminal(N::Y);
    grammar.add_rule(N::S).nonterminal(N::U);
    grammar.add_rule(N::X).terminal(T::A);
    grammar.add_rule(N::X).nonterminal(N::X).terminal(T::A);
    grammar.add_rule(N::X).terminal(T::B).nonterminal(N::W);
    grammar.add_rule(N::Y).nonterminal(N::Z);
    grammar.add_rule(N::Z).nonterminal(N::Y).terminal(T::A);
    grammar.add_rule(N::Z).nonterminal(N::W);
    grammar.add_rule(N::W).nonterminal(N::Y);
    grammar.add_rule(N::W).terminal(T::C);
    grammar.add_rule(N::U).nonterminal(N::V);
    grammar.add_rule(N::U).terminal(T::B);
    grammar.add_rule(N::V).nonterminal(N::U);

    let error = grammar.validate().unwrap_err();
    assert_eq!(
        Error::ContainsCycles(vec![vec![N::U, N::V], vec![N::Y, N::Z, N::W]]),
        error
    );
}

#[test]
fn can_display_grammar() {
    define_nonterminal!(N: [S, X, Y]);
    define_terminal!(T: [C, D]);
    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::X).terminal(T::C);
    grammar.add_rule(N::Y).terminal(T::D);
    grammar
        .add_rule(N::X)
        .terminal(T::D)
        .nonterminal(N::Y)
        .terminal(T::C);

    const EXPECTED: &str = "S -> X\nX -> C\nY -> D\nX -> D Y C\n";
    let actual = grammar.to_string();
    assert_eq!(EXPECTED, actual);
}

#[test]
fn can_compute_first_sets_of_terminal_only_grammar() {
    define_nonterminal!(N: [S]);
    define_terminal!(T: [A, B, C]);
    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).terminal(T::A);
    grammar.add_rule(N::S).terminal(T::B);
    grammar.add_rule(N::S).terminal(T::C);

    let grammar = grammar.validate().unwrap();
    let first_start = grammar.first(&[Symbol::Nonterminal(N::S)]);
    assert_eq!(vec![T::A, T::B, T::C], first_start);
}

#[test]
fn can_compute_first_sets_of_example_grammar() {
    define_nonterminal!(N: [S, X, Y, Z]);
    define_terminal!(T: [A, B, C, D, E]);
    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::S).nonterminal(N::Y);
    grammar.add_rule(N::X).terminal(T::A).nonterminal(N::X);
    grammar.add_rule(N::X).terminal(T::B).nonterminal(N::X);
    grammar.add_rule(N::X).terminal(T::D);
    grammar.add_rule(N::Y).nonterminal(N::Y).nonterminal(N::Z);
    grammar.add_rule(N::Y).nonterminal(N::Z).terminal(T::E);
    grammar.add_rule(N::Z).terminal(T::C).nonterminal(N::Z);
    grammar.add_rule(N::Z).terminal(T::D);

    // I've left out tests for FIRST of more than 1 symbols. Since this operation is over a
    // proper grammar, where nullable symbols are not allowed, the FIRST algorithm never needs
    // to check more than 1 symbol from its input.

    let grammar = grammar.validate().unwrap();
    let first_empty = grammar.first(&[]);
    let first_start = grammar.first(&[Symbol::Nonterminal(N::S)]);
    let first_a = grammar.first(&[Symbol::Terminal(T::A)]);
    let first_b = grammar.first(&[Symbol::Terminal(T::B)]);
    let first_c = grammar.first(&[Symbol::Terminal(T::C)]);
    let first_d = grammar.first(&[Symbol::Terminal(T::D)]);
    let first_e = grammar.first(&[Symbol::Terminal(T::E)]);
    let first_x = grammar.first(&[Symbol::Nonterminal(N::X)]);
    let first_y = grammar.first(&[Symbol::Nonterminal(N::Y)]);
    let first_z = grammar.first(&[Symbol::Nonterminal(N::Z)]);

    assert!(first_empty.is_empty());
    assert_eq!(vec![T::A, T::B, T::C, T::D], first_start);
    assert_eq!(vec![T::A], first_a);
    assert_eq!(vec![T::B], first_b);
    assert_eq!(vec![T::C], first_c);
    assert_eq!(vec![T::D], first_d);
    assert_eq!(vec![T::E], first_e);
    assert_eq!(vec![T::A, T::B, T::D], first_x);
    assert_eq!(vec![T::C, T::D], first_y);
    assert_eq!(vec![T::C, T::D], first_z);
}

#[test]
fn can_compute_follow_set_of_example_grammar() {
    define_nonterminal!(N: [S, X, Y, Z]);
    define_terminal!(T: [A, B, C, D]);
    let mut grammar = Grammar::new(N::S);

    grammar.add_rule(N::S).nonterminal(N::X).nonterminal(N::Y);
    grammar.add_rule(N::S).nonterminal(N::X).nonterminal(N::Z);
    grammar.add_rule(N::X).terminal(T::D).nonterminal(N::X);
    grammar.add_rule(N::X).terminal(T::D);
    grammar.add_rule(N::Y).nonterminal(N::Y).terminal(T::A);
    grammar.add_rule(N::Y).terminal(T::B);
    grammar.add_rule(N::Z).nonterminal(N::Z).terminal(T::C);
    grammar.add_rule(N::Z).terminal(T::D);

    let grammar = grammar.validate().unwrap();

    assert_eq!(vec![None], grammar.follow(N::S));
    assert_eq!(vec![Some(T::B), Some(T::D)], grammar.follow(N::X));
    assert_eq!(vec![None, Some(T::A)], grammar.follow(N::Y));
    assert_eq!(vec![None, Some(T::C)], grammar.follow(N::Z));
}

define_nonterminal!(ExampleNonterminal: [Expr, Term, Factor]);
define_terminal!(ExampleTerminal: [Plus, Asterisk, X, Open, Close]);

fn simple_example_grammar() -> ParseTable<ExampleNonterminal, ExampleTerminal> {
    // S' -> E
    // E -> E + T
    // E -> T
    // T -> T * F
    // T -> F
    // F -> x
    // F -> ( E )
    use {ExampleNonterminal::*, ExampleTerminal::*};
    let mut grammar = Grammar::new(Expr);
    grammar
        .add_rule(Expr)
        .nonterminal(Expr)
        .terminal(Plus)
        .nonterminal(Term);
    grammar.add_rule(Expr).nonterminal(Term);
    grammar
        .add_rule(Term)
        .nonterminal(Term)
        .terminal(Asterisk)
        .nonterminal(Factor);
    grammar.add_rule(Term).nonterminal(Factor);
    grammar.add_rule(Factor).terminal(X);
    grammar
        .add_rule(Factor)
        .terminal(Open)
        .nonterminal(Expr)
        .terminal(Close);
    let grammar = grammar.validate().unwrap();
    grammar.parse_table().unwrap()
}

#[test]
fn can_generate_parse_table_for_simple_grammar() {
    use {ExampleNonterminal::*, ExampleTerminal::*};
    let table = simple_example_grammar();

    use ParseAction::*;
    let actions = &[
        (0, Some(X), Shift(4)),
        (0, Some(Open), Shift(5)),
        (1, Some(Plus), Shift(6)),
        (1, None, Accept),
        (2, Some(Plus), Reduce(Expr, 1)),
        (2, Some(Asterisk), Shift(7)),
        (2, Some(Close), Reduce(Expr, 1)),
        (2, None, Reduce(Expr, 1)),
        (3, Some(Plus), Reduce(Term, 1)),
        (3, Some(Asterisk), Reduce(Term, 1)),
        (3, Some(Close), Reduce(Term, 1)),
        (3, None, Reduce(Term, 1)),
        (4, Some(Plus), Reduce(Factor, 1)),
        (4, Some(Asterisk), Reduce(Factor, 1)),
        (4, Some(Close), Reduce(Factor, 1)),
        (4, None, Reduce(Factor, 1)),
        (5, Some(X), Shift(4)),
        (5, Some(Open), Shift(5)),
        (6, Some(X), Shift(4)),
        (6, Some(Open), Shift(5)),
        (7, Some(X), Shift(4)),
        (7, Some(Open), Shift(5)),
        (8, Some(Plus), Shift(6)),
        (8, Some(Close), Shift(11)),
        (9, Some(Plus), Reduce(Expr, 3)),
        (9, Some(Asterisk), Shift(7)),
        (9, Some(Close), Reduce(Expr, 3)),
        (9, None, Reduce(Expr, 3)),
        (10, Some(Plus), Reduce(Term, 3)),
        (10, Some(Asterisk), Reduce(Term, 3)),
        (10, Some(Close), Reduce(Term, 3)),
        (10, None, Reduce(Term, 3)),
        (11, Some(Plus), Reduce(Factor, 3)),
        (11, Some(Asterisk), Reduce(Factor, 3)),
        (11, Some(Close), Reduce(Factor, 3)),
        (11, None, Reduce(Factor, 3)),
    ];

    for &(state, input, expected) in actions {
        let actual = table.action(state, input);
        assert_eq!(expected, actual, "state: {state}, input: {input:?}");
    }

    let gotos = &[
        (0, Expr, 1),
        (0, Term, 2),
        (0, Factor, 3),
        (5, Expr, 8),
        (5, Term, 2),
        (5, Factor, 3),
        (6, Term, 9),
        (6, Factor, 3),
        (7, Factor, 10),
    ];

    for &(state, nonterminal, expected) in gotos {
        let actual = table.goto(state, nonterminal);
        assert_eq!(
            expected, actual,
            "state: {state}, nonterminal: {nonterminal:?}"
        );
    }
}

#[test]
fn can_report_shift_reduce_conflict_in_table() {
    define_nonterminal!(N: [S, X]);
    define_terminal!(T: [A, B, C]);
    let mut grammar = Grammar::new(N::S);
    grammar
        .add_rule(N::S)
        .terminal(T::A)
        .nonterminal(N::X)
        .terminal(T::B);
    grammar.add_rule(N::X).terminal(T::C);
    grammar.add_rule(N::X).terminal(T::C).terminal(T::B);
    let grammar = grammar.validate().unwrap();
    let error = grammar.parse_table().unwrap_err();

    assert_eq!(ParseTableConflict::ShiftReduce, error);
}

#[test]
fn can_report_reduce_reduce_conflict_in_table() {
    define_nonterminal!(N: [S, X, Y]);
    define_terminal!(T: [A]);
    let mut grammar = Grammar::new(N::S);
    grammar.add_rule(N::S).nonterminal(N::X);
    grammar.add_rule(N::S).nonterminal(N::Y);
    grammar.add_rule(N::X).terminal(T::A);
    grammar.add_rule(N::Y).terminal(T::A);
    let grammar = grammar.validate().unwrap();
    let error = grammar.parse_table().unwrap_err();
    assert_eq!(ParseTableConflict::ReduceReduce, error);
}

#[test]
fn can_parse_simple_input() {
    use {ExampleNonterminal::*, ExampleTerminal::*};
    let table = simple_example_grammar();

    #[derive(Debug, PartialEq, Eq)]
    struct Node(
        ExampleNonterminal,
        Vec<ParseTree<Node, ExampleTerminal, i32>>,
    );

    // (x + x) * (x + x) + x
    let input = [
        (Open, 0),
        (X, 1),
        (Plus, 2),
        (X, 3),
        (Close, 4),
        (Asterisk, 5),
        (Open, 6),
        (X, 7),
        (Plus, 8),
        (X, 9),
        (Close, 10),
        (Plus, 11),
        (X, 12),
    ]
    .into_iter();

    fn reduce<'a>(
        head: ExampleNonterminal,
        body: Vec<ParseTree<Node, ExampleTerminal, i32>>,
    ) -> Node {
        Node(head, body)
    }

    use ParseTree::{Nonterminal as N, Terminal as T};
    #[rustfmt::skip]
    let expected = Node(Expr, vec![
        N(Node(Expr, vec![
            N(Node(Term, vec![
                N(Node(Term, vec![
                    N(Node(Factor, vec![
                        T(Open, 0),
                        N(Node(Expr, vec![
                            N(Node(Expr, vec![
                                N(Node(Term, vec![
                                    N(Node(Factor, vec![
                                        T(X, 1),
                                    ])),
                                ]))
                            ])),
                            T(Plus, 2),
                            N(Node(Term, vec![
                                N(Node(Factor, vec![
                                    T(X, 3),
                                ])),
                            ]))
                        ])),
                        T(Close, 4),
                    ])),
                ])),
                T(Asterisk, 5),
                N(Node(Factor, vec![
                    T(Open, 6),
                    N(Node(Expr, vec![
                        N(Node(Expr, vec![
                            N(Node(Term, vec![
                                N(Node(Factor, vec![
                                    T(X, 7),
                                ])),
                            ]))
                        ])),
                        T(Plus, 8),
                        N(Node(Term, vec![
                            N(Node(Factor, vec![
                                T(X, 9),
                            ])),
                        ]))
                    ])),
                    T(Close, 10),
                ])),
            ])),
        ])),
        T(Plus, 11),
        N(Node(Term, vec![
            N(Node(Factor, vec![
                T(X, 12),
            ])),
        ])),
    ]);

    assert_eq!(expected, table.parse(input, reduce).unwrap());
}

#[test]
fn can_report_syntax_error() {
    let table = simple_example_grammar();

    #[derive(Debug, PartialEq, Eq)]
    struct Node(
        ExampleNonterminal,
        Vec<ParseTree<Node, ExampleTerminal, i32>>,
    );

    fn reduce<'a>(
        head: ExampleNonterminal,
        body: Vec<ParseTree<Node, ExampleTerminal, i32>>,
    ) -> Node {
        Node(head, body)
    }

    // (x + x) * + x
    use ExampleTerminal::*;
    let input = [
        (Open, 0),
        (X, 1),
        (Plus, 2),
        (X, 3),
        (Close, 4),
        (Asterisk, 5),
        (Plus, 6),
        (X, 7),
    ]
    .into_iter();

    let expected = {
        let mut set = HashSet::new();
        set.insert(Some(Open));
        set.insert(Some(X));
        set
    };

    let (error, position) = table.parse(input, reduce).unwrap_err();
    assert_eq!(Some(Plus), error.actual());
    assert_eq!(&expected, error.expected());
    assert_eq!(Some(6), position);
}
