use grammar::{Error, Grammar, Nonterminal, ProperGrammar, Symbol, Terminal};

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
