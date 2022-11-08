use grammar::{
    Error, Grammar, Nonterminal, ParseAction, ParseTable, ParseTableConflict, ParseTree,
    ProperGrammar, Symbol, Terminal,
};

#[test]
fn can_create_grammar() {
    let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();

    let a: Nonterminal = grammar.add_nonterminal("A");
    assert_ne!(start, a);

    let b: Nonterminal = grammar.add_nonterminal("B");
    assert_ne!(start, b);
    assert_ne!(a, b);

    let c: Terminal = grammar.add_terminal("c");
    let d: Terminal = grammar.add_terminal("d");
    assert_ne!(c, d);

    assert_eq!(0, grammar.rule_count());

    grammar.add_rule(a).terminal(c);
    assert_eq!(1, grammar.rule_count());

    grammar.add_rule(b).terminal(d);
    assert_eq!(2, grammar.rule_count());

    grammar.add_rule(a).terminal(d).nonterminal(b).terminal(c);
    assert_eq!(3, grammar.rule_count());

    grammar.add_rule(start).nonterminal(a);
    assert_eq!(4, grammar.rule_count());

    let _proper_grammar: ProperGrammar = grammar.validate().unwrap();
}

#[test]
fn unproductive_grammar_is_not_proper() {
    let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
    // S -> X
    // X -> a
    // X -> Y

    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let a = grammar.add_terminal("a");

    grammar.add_rule(start).nonterminal(x);
    grammar.add_rule(x).terminal(a);
    grammar.add_rule(x).nonterminal(y);

    let error: Error = grammar.validate().unwrap_err();
    assert_eq!(Error::UnproductiveNonterminals(vec![y]), error);
}

#[test]
fn unreachable_grammar_is_not_proper() {
    let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
    // S -> X
    // X -> a
    // Y -> b
    // unreachable: b, Y, c

    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");

    grammar.add_rule(start).nonterminal(x);
    grammar.add_rule(x).terminal(a);
    grammar.add_rule(y).terminal(b);

    let error: Error = grammar.validate().unwrap_err();
    assert_eq!(Error::UnreachableSymbols(vec![y], vec![b, c]), error);
}

#[test]
fn epsilon_productions_are_not_proper() {
    let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
    // S -> X
    // S -> Y
    // S -> Z
    // X ->
    // X -> a X
    // Y -> b Z c
    // Z ->

    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let z = grammar.add_nonterminal("Z");
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");

    grammar.add_rule(start).nonterminal(x);
    grammar.add_rule(start).nonterminal(y);
    grammar.add_rule(start).nonterminal(z);
    grammar.add_rule(z);
    grammar.add_rule(x);
    grammar.add_rule(x).terminal(a).nonterminal(x);
    grammar.add_rule(y).terminal(b).nonterminal(z).terminal(c);

    let error: Error = grammar.validate().unwrap_err();
    assert_eq!(Error::EpsilonProductions(vec![x, z]), error);
}

#[test]
fn cycles_are_not_proper() {
    let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
    // S -> X
    // S -> Y
    // S -> U
    // X -> a
    // X -> X a
    // X -> b W
    // Y -> Z
    // Z -> Y a
    // Z -> W
    // W -> Y
    // W -> c
    // U -> V
    // V -> U
    // U -> b

    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let z = grammar.add_nonterminal("Z");
    let w = grammar.add_nonterminal("W");
    let u = grammar.add_nonterminal("U");
    let v = grammar.add_nonterminal("V");
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");

    grammar.add_rule(start).nonterminal(x);
    grammar.add_rule(start).nonterminal(y);
    grammar.add_rule(start).nonterminal(u);
    grammar.add_rule(x).terminal(a);
    grammar.add_rule(x).nonterminal(x).terminal(a);
    grammar.add_rule(x).terminal(b).nonterminal(w);
    grammar.add_rule(y).nonterminal(z);
    grammar.add_rule(z).nonterminal(y).terminal(a);
    grammar.add_rule(z).nonterminal(w);
    grammar.add_rule(w).nonterminal(y);
    grammar.add_rule(w).terminal(c);
    grammar.add_rule(u).nonterminal(v);
    grammar.add_rule(u).terminal(b);
    grammar.add_rule(v).nonterminal(u);

    let error: Error = grammar.validate().unwrap_err();
    assert_eq!(
        Error::ContainsCycles(vec![vec![y, z, w], vec![u, v]]),
        error
    );
}

#[test]
fn can_display_grammar() {
    let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();

    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let c = grammar.add_terminal("c");
    let d = grammar.add_terminal("d");

    grammar.add_rule(start).nonterminal(x);
    grammar.add_rule(x).terminal(c);
    grammar.add_rule(y).terminal(d);
    grammar.add_rule(x).terminal(d).nonterminal(y).terminal(c);

    const EXPECTED: &str = "start -> X\nX -> c\nY -> d\nX -> d Y c\n";
    let actual = grammar.to_string();
    assert_eq!(EXPECTED, actual);
}

#[test]
fn can_compute_first_sets_of_terminal_only_grammar() {
    let (start, mut grammar) = Grammar::new();
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");
    grammar.add_rule(start).terminal(a);
    grammar.add_rule(start).terminal(b);
    grammar.add_rule(start).terminal(c);

    let grammar = grammar.validate().unwrap();
    let first_start = grammar.first(&[Symbol::Nonterminal(start)]);
    assert_eq!(vec![a, b, c], first_start);
}

#[test]
fn can_compute_first_sets_of_example_grammar() {
    let (start, mut grammar) = Grammar::new();
    // S -> X
    // S -> Y
    // X -> a X
    // X -> b X
    // X -> d
    // Y -> Y Z
    // Y -> Z e
    // Z -> c Z
    // Z -> d

    let x = grammar.add_nonterminal("x");
    let y = grammar.add_nonterminal("y");
    let z = grammar.add_nonterminal("z");
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");
    let d = grammar.add_terminal("d");
    let e = grammar.add_terminal("d");
    grammar.add_rule(start).nonterminal(x);
    grammar.add_rule(start).nonterminal(y);
    grammar.add_rule(x).terminal(a).nonterminal(x);
    grammar.add_rule(x).terminal(b).nonterminal(x);
    grammar.add_rule(x).terminal(d);
    grammar.add_rule(y).nonterminal(y).nonterminal(z);
    grammar.add_rule(y).nonterminal(z).terminal(e);
    grammar.add_rule(z).terminal(c).nonterminal(z);
    grammar.add_rule(z).terminal(d);

    // I've left out tests for FIRST of more than 1 symbols. Since this operation is over a
    // proper grammar, where nullable symbols are not allowed, the FIRST algorithm never needs
    // to check more than 1 symbol from its input.

    let grammar = grammar.validate().unwrap();
    let first_empty = grammar.first(&[]);
    let first_start = grammar.first(&[Symbol::Nonterminal(start)]);
    let first_a = grammar.first(&[Symbol::Terminal(a)]);
    let first_b = grammar.first(&[Symbol::Terminal(b)]);
    let first_c = grammar.first(&[Symbol::Terminal(c)]);
    let first_d = grammar.first(&[Symbol::Terminal(d)]);
    let first_e = grammar.first(&[Symbol::Terminal(e)]);
    let first_x = grammar.first(&[Symbol::Nonterminal(x)]);
    let first_y = grammar.first(&[Symbol::Nonterminal(y)]);
    let first_z = grammar.first(&[Symbol::Nonterminal(z)]);

    assert!(first_empty.is_empty());
    assert_eq!(vec![a, b, c, d], first_start);
    assert_eq!(vec![a], first_a);
    assert_eq!(vec![b], first_b);
    assert_eq!(vec![c], first_c);
    assert_eq!(vec![d], first_d);
    assert_eq!(vec![e], first_e);
    assert_eq!(vec![a, b, d], first_x);
    assert_eq!(vec![c, d], first_y);
    assert_eq!(vec![c, d], first_z);
}

#[test]
fn can_compute_follow_set_of_example_grammar() {
    let (start, mut grammar) = Grammar::new();
    // S -> X Y
    // S -> X Z
    // X -> d X
    // X -> d
    // Y -> Y a
    // Y -> b
    // Z -> Z c
    // Z -> d

    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let z = grammar.add_nonterminal("Z");
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");
    let d = grammar.add_terminal("d");

    grammar.add_rule(start).nonterminal(x).nonterminal(y);
    grammar.add_rule(start).nonterminal(x).nonterminal(z);
    grammar.add_rule(x).terminal(d).nonterminal(x);
    grammar.add_rule(x).terminal(d);
    grammar.add_rule(y).nonterminal(y).terminal(a);
    grammar.add_rule(y).terminal(b);
    grammar.add_rule(z).nonterminal(z).terminal(c);
    grammar.add_rule(z).terminal(d);

    let grammar = grammar.validate().unwrap();

    assert_eq!(vec![None], grammar.follow(start));
    assert_eq!(vec![Some(b), Some(d)], grammar.follow(x));
    assert_eq!(vec![None, Some(a)], grammar.follow(y));
    assert_eq!(vec![None, Some(c)], grammar.follow(z));
}

#[test]
fn can_get_symbol_names() {
    let (start, mut grammar) = Grammar::new();
    let t = grammar.add_terminal("terminal");
    let s = grammar.add_nonterminal("Example");
    let u = grammar.add_terminal("Terminal2");
    grammar.add_rule(start).nonterminal(s);
    grammar.add_rule(s).terminal(t).terminal(u);
    let proper_grammar = grammar.validate().unwrap();

    assert_eq!("start", proper_grammar.nonterminal_name(start));
    assert_eq!("terminal", proper_grammar.terminal_name(t));
    assert_eq!("Example", proper_grammar.nonterminal_name(s));
    assert_eq!("Terminal2", proper_grammar.terminal_name(u));
    assert_eq!(
        "Example",
        proper_grammar.symbol_name(Symbol::Nonterminal(s))
    );
    assert_eq!("Terminal2", proper_grammar.symbol_name(Symbol::Terminal(u)));
}

#[test]
fn can_generate_parse_table_for_minimal_grammar() {
    // S -> a
    let (s, mut grammar) = Grammar::new();
    let a = grammar.add_terminal("a");
    grammar.add_rule(s).terminal(a);
    let grammar = grammar.validate().unwrap();

    let table: ParseTable = grammar.parse_table().unwrap();

    assert_eq!(ParseAction::Shift(2), table.action(0, Some(a)));
    assert_eq!(ParseAction::Error, table.action(0, None));
    assert_eq!(ParseAction::Error, table.action(1, Some(a)));
    assert_eq!(ParseAction::Accept, table.action(1, None));
    assert_eq!(ParseAction::Error, table.action(2, Some(a)));
    assert_eq!(ParseAction::Reduce(s, 1), table.action(2, None));

    assert_eq!(1, table.goto(0, s));
}

#[test]
fn can_generate_parse_table_for_simple_grammar() {
    // S' -> E
    // E -> E + T
    // E -> T
    // T -> T * F
    // T -> F
    // F -> x
    // F -> ( E )
    let (expr, mut grammar) = Grammar::new();
    let term = grammar.add_nonterminal("T");
    let factor = grammar.add_nonterminal("F");
    let plus = grammar.add_terminal("+");
    let asterisk = grammar.add_terminal("*");
    let x = grammar.add_terminal("x");
    let open = grammar.add_terminal("(");
    let close = grammar.add_terminal(")");
    grammar
        .add_rule(expr)
        .nonterminal(expr)
        .terminal(plus)
        .nonterminal(term);
    grammar.add_rule(expr).nonterminal(term);
    grammar
        .add_rule(term)
        .nonterminal(term)
        .terminal(asterisk)
        .nonterminal(factor);
    grammar.add_rule(term).nonterminal(factor);
    grammar.add_rule(factor).terminal(x);
    grammar
        .add_rule(factor)
        .terminal(open)
        .nonterminal(expr)
        .terminal(close);
    let grammar = grammar.validate().unwrap();
    let table = grammar.parse_table().unwrap();

    use ParseAction::*;
    let actions: &[(usize, Option<Terminal>, ParseAction)] = &[
        (0, Some(x), Shift(4)),
        (0, Some(open), Shift(5)),
        (1, Some(plus), Shift(6)),
        (1, None, Accept),
        (2, Some(plus), Reduce(expr, 1)),
        (2, Some(asterisk), Shift(7)),
        (2, Some(close), Reduce(expr, 1)),
        (2, None, Reduce(expr, 1)),
        (3, Some(plus), Reduce(term, 1)),
        (3, Some(asterisk), Reduce(term, 1)),
        (3, Some(close), Reduce(term, 1)),
        (3, None, Reduce(term, 1)),
        (4, Some(plus), Reduce(factor, 1)),
        (4, Some(asterisk), Reduce(factor, 1)),
        (4, Some(close), Reduce(factor, 1)),
        (4, None, Reduce(factor, 1)),
        (5, Some(x), Shift(4)),
        (5, Some(open), Shift(5)),
        (6, Some(x), Shift(4)),
        (6, Some(open), Shift(5)),
        (7, Some(x), Shift(4)),
        (7, Some(open), Shift(5)),
        (8, Some(plus), Shift(6)),
        (8, Some(close), Shift(11)),
        (9, Some(plus), Reduce(expr, 3)),
        (9, Some(asterisk), Shift(7)),
        (9, Some(close), Reduce(expr, 3)),
        (9, None, Reduce(expr, 3)),
        (10, Some(plus), Reduce(term, 3)),
        (10, Some(asterisk), Reduce(term, 3)),
        (10, Some(close), Reduce(term, 3)),
        (10, None, Reduce(term, 3)),
        (11, Some(plus), Reduce(factor, 3)),
        (11, Some(asterisk), Reduce(factor, 3)),
        (11, Some(close), Reduce(factor, 3)),
        (11, None, Reduce(factor, 3)),
    ];

    for &(state, input, expected) in actions {
        let actual = table.action(state, input);
        assert_eq!(expected, actual, "state: {state}, input: {input:?}");
    }

    let gotos: &[(usize, Nonterminal, usize)] = &[
        (0, expr, 1),
        (0, term, 2),
        (0, factor, 3),
        (5, expr, 8),
        (5, term, 2),
        (5, factor, 3),
        (6, term, 9),
        (6, factor, 3),
        (7, factor, 10),
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
    // S -> a X b
    // X -> c
    // X -> c b
    let (s, mut grammar) = Grammar::new();
    let x = grammar.add_nonterminal("X");
    let a = grammar.add_terminal("a");
    let b = grammar.add_terminal("b");
    let c = grammar.add_terminal("c");
    grammar.add_rule(s).terminal(a).nonterminal(x).terminal(b);
    grammar.add_rule(x).terminal(c);
    grammar.add_rule(x).terminal(c).terminal(b);
    let grammar = grammar.validate().unwrap();
    let error = grammar.parse_table().unwrap_err();

    assert_eq!(ParseTableConflict::ShiftReduce, error);
}

#[test]
fn can_report_reduce_reduce_conflict_in_table() {
    // S -> X
    // S -> Y
    // X -> a
    // Y -> a
    let (s, mut grammar) = Grammar::new();
    let x = grammar.add_nonterminal("X");
    let y = grammar.add_nonterminal("Y");
    let a = grammar.add_terminal("a");
    grammar.add_rule(s).nonterminal(x);
    grammar.add_rule(s).nonterminal(y);
    grammar.add_rule(x).terminal(a);
    grammar.add_rule(y).terminal(a);
    let grammar = grammar.validate().unwrap();
    let error = grammar.parse_table().unwrap_err();
    assert_eq!(ParseTableConflict::ReduceReduce, error);
}

#[test]
fn can_parse_simple_input() {
    // S' -> E
    // E -> E + T
    // E -> T
    // T -> T * F
    // T -> F
    // F -> x
    // F -> ( E )
    let (expr, mut grammar) = Grammar::new();
    let term = grammar.add_nonterminal("T");
    let factor = grammar.add_nonterminal("F");
    let plus = grammar.add_terminal("+");
    let asterisk = grammar.add_terminal("*");
    let x = grammar.add_terminal("x");
    let open = grammar.add_terminal("(");
    let close = grammar.add_terminal(")");
    grammar
        .add_rule(expr)
        .nonterminal(expr)
        .terminal(plus)
        .nonterminal(term);
    grammar.add_rule(expr).nonterminal(term);
    grammar
        .add_rule(term)
        .nonterminal(term)
        .terminal(asterisk)
        .nonterminal(factor);
    grammar.add_rule(term).nonterminal(factor);
    grammar.add_rule(factor).terminal(x);
    grammar
        .add_rule(factor)
        .terminal(open)
        .nonterminal(expr)
        .terminal(close);
    let grammar = grammar.validate().unwrap();
    let table = grammar.parse_table().unwrap();

    // (x + x) * (x + x) + x
    let input = [
        open, x, plus, x, close, asterisk, open, x, plus, x, close, plus, x,
    ]
    .into_iter();

    use ParseTree::{Nonterminal as N, Terminal as T};
    #[rustfmt::skip]
    let expected: ParseTree = N(expr, vec![
        N(expr, vec![
            N(term, vec![
                N(term, vec![
                    N(factor, vec![
                        T(open),
                        N(expr, vec![
                            N(expr, vec![
                                N(term, vec![
                                    N(factor, vec![
                                        T(x),
                                    ]),
                                ])
                            ]),
                            T(plus),
                            N(term, vec![
                                N(factor, vec![
                                    T(x),
                                ]),
                            ])
                        ]),
                        T(close),
                    ]),
                ]),
                T(asterisk),
                N(factor, vec![
                    T(open),
                    N(expr, vec![
                        N(expr, vec![
                            N(term, vec![
                                N(factor, vec![
                                    T(x),
                                ]),
                            ])
                            ]),
                        T(plus),
                        N(term, vec![
                            N(factor, vec![
                                T(x),
                            ]),
                        ])
                    ]),
                    T(close),
                ]),
            ]),
        ]),
        T(plus),
        N(term, vec![
            N(factor, vec![
                T(x),
            ]),
        ]),
    ]);

    let parse_tree: ParseTree = table.parse(input);
    assert_eq!(expected, parse_tree);
}
