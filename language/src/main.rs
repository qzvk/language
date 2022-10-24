#![forbid(unsafe_code)]

use std::io::{stdin, Read};

use grammar::{Grammar, ProperGrammar};

fn generate_grammar() -> ProperGrammar {
    let (document, mut grammar) = Grammar::new();

    let plus = grammar.add_terminal();
    let minus = grammar.add_terminal();
    let asterisk = grammar.add_terminal();
    let slash = grammar.add_terminal();
    let semicolon = grammar.add_terminal();
    let equals = grammar.add_terminal();
    let open_paren = grammar.add_terminal();
    let close_paren = grammar.add_terminal();
    let integer = grammar.add_terminal();
    let ident = grammar.add_terminal();

    let assignment_seq = grammar.add_nonterminal();
    let assignment = grammar.add_nonterminal();
    let ident_seq = grammar.add_nonterminal();
    let mul_expr = grammar.add_nonterminal();
    let add_expr = grammar.add_nonterminal();
    let apply_expr = grammar.add_nonterminal();
    let unary_expr = grammar.add_nonterminal();

    grammar.add_rule(document).nonterminal(mul_expr);
    grammar
        .add_rule(document)
        .nonterminal(assignment_seq)
        .nonterminal(mul_expr);

    grammar.add_rule(assignment_seq).nonterminal(assignment);
    grammar
        .add_rule(assignment_seq)
        .nonterminal(assignment)
        .nonterminal(assignment_seq);

    grammar
        .add_rule(assignment)
        .nonterminal(ident_seq)
        .terminal(equals)
        .nonterminal(mul_expr)
        .terminal(semicolon);

    grammar.add_rule(ident_seq).terminal(ident);
    grammar
        .add_rule(ident_seq)
        .terminal(ident)
        .nonterminal(ident_seq);

    grammar
        .add_rule(mul_expr)
        .nonterminal(mul_expr)
        .terminal(asterisk)
        .nonterminal(add_expr);
    grammar
        .add_rule(mul_expr)
        .nonterminal(mul_expr)
        .terminal(slash)
        .nonterminal(add_expr);
    grammar.add_rule(mul_expr).nonterminal(add_expr);

    grammar
        .add_rule(add_expr)
        .nonterminal(add_expr)
        .terminal(plus)
        .nonterminal(apply_expr);
    grammar
        .add_rule(add_expr)
        .nonterminal(add_expr)
        .terminal(minus)
        .nonterminal(apply_expr);
    grammar.add_rule(add_expr).nonterminal(apply_expr);

    grammar
        .add_rule(apply_expr)
        .nonterminal(apply_expr)
        .nonterminal(unary_expr);
    grammar.add_rule(apply_expr).nonterminal(unary_expr);

    grammar
        .add_rule(unary_expr)
        .terminal(open_paren)
        .nonterminal(mul_expr)
        .terminal(close_paren);
    grammar.add_rule(unary_expr).terminal(integer);
    grammar.add_rule(unary_expr).terminal(ident);

    grammar.validate().unwrap()
}

fn main() {
    let _grammar = generate_grammar();

    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    for (result, info) in lexer::lex(&input) {
        match result {
            Ok(kind) => println!("{info} {kind}"),
            Err(error) => println!("{info} {error}"),
        }
    }
}
