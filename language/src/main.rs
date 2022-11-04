#![forbid(unsafe_code)]

use std::io::{stdin, Read};

use grammar::{Grammar, ProperGrammar, Symbol};

fn generate_grammar() -> ProperGrammar {
    let (document, mut grammar) = Grammar::new();

    let plus = grammar.add_terminal("'+'");
    let minus = grammar.add_terminal("'-'");
    let asterisk = grammar.add_terminal("'*'");
    let slash = grammar.add_terminal("'/'");
    let semicolon = grammar.add_terminal("';'");
    let equals = grammar.add_terminal("'='");
    let open_paren = grammar.add_terminal("'('");
    let close_paren = grammar.add_terminal("')'");
    let integer = grammar.add_terminal("integer");
    let ident = grammar.add_terminal("ident");

    let assignment_seq = grammar.add_nonterminal("assignment-seq");
    let assignment = grammar.add_nonterminal("assignment");
    let ident_seq = grammar.add_nonterminal("ident-seq");
    let add_expr = grammar.add_nonterminal("add-expr");
    let mul_expr = grammar.add_nonterminal("mul-expr");
    let apply_expr = grammar.add_nonterminal("apply-expr");
    let unary_expr = grammar.add_nonterminal("unary-expr");
    let add_op = grammar.add_nonterminal("add-op");
    let mul_op = grammar.add_nonterminal("mul-op");

    grammar.add_rule(document).nonterminal(add_expr);
    grammar
        .add_rule(document)
        .nonterminal(assignment_seq)
        .nonterminal(add_expr);

    grammar.add_rule(assignment_seq).nonterminal(assignment);
    grammar
        .add_rule(assignment_seq)
        .nonterminal(assignment)
        .nonterminal(assignment_seq);

    grammar
        .add_rule(assignment)
        .nonterminal(ident_seq)
        .terminal(equals)
        .nonterminal(add_expr)
        .terminal(semicolon);

    grammar.add_rule(ident_seq).terminal(ident);
    grammar
        .add_rule(ident_seq)
        .terminal(ident)
        .nonterminal(ident_seq);

    grammar
        .add_rule(add_expr)
        .nonterminal(add_expr)
        .nonterminal(add_op)
        .nonterminal(mul_expr);
    grammar.add_rule(add_expr).nonterminal(mul_expr);

    grammar
        .add_rule(mul_expr)
        .nonterminal(mul_expr)
        .nonterminal(mul_op)
        .nonterminal(apply_expr);
    grammar.add_rule(mul_expr).nonterminal(apply_expr);

    grammar
        .add_rule(apply_expr)
        .nonterminal(apply_expr)
        .nonterminal(unary_expr);
    grammar.add_rule(apply_expr).nonterminal(unary_expr);

    grammar
        .add_rule(unary_expr)
        .terminal(open_paren)
        .nonterminal(add_expr)
        .terminal(close_paren);
    grammar.add_rule(unary_expr).terminal(integer);
    grammar.add_rule(unary_expr).terminal(ident);

    grammar.add_rule(add_op).terminal(plus);
    grammar.add_rule(add_op).terminal(minus);

    grammar.add_rule(mul_op).terminal(asterisk);
    grammar.add_rule(mul_op).terminal(slash);

    println!("################################################################################");
    println!("#                                    Grammar                                   #");
    println!("################################################################################");
    print!("{}", grammar);

    let proper_grammar = grammar.validate().unwrap();
    let terminals = [
        plus,
        minus,
        asterisk,
        slash,
        semicolon,
        equals,
        open_paren,
        close_paren,
        integer,
        ident,
    ];
    let nonterminals = [
        document,
        assignment_seq,
        assignment,
        ident_seq,
        add_expr,
        mul_expr,
        apply_expr,
        unary_expr,
        add_op,
        mul_op,
    ];

    println!();
    println!("################################################################################");
    println!("#                                    Firsts                                    #");
    println!("################################################################################");

    for n in terminals
        .into_iter()
        .map(Symbol::Terminal)
        .chain(nonterminals.iter().cloned().map(Symbol::Nonterminal))
    {
        let name = proper_grammar.symbol_name(n);
        print!("FIRST({name}) = {{ ");

        for t in proper_grammar.first(&[n]) {
            let name = proper_grammar.terminal_name(t);
            print!("{name}, ");
        }
        println!("}}");
    }

    println!();
    println!("################################################################################");
    println!("#                                    Follows                                   #");
    println!("################################################################################");

    for n in nonterminals {
        let name = proper_grammar.nonterminal_name(n);
        print!("FOLLOW({name}) = {{ ");

        for t in proper_grammar.follow(n) {
            let name = match t {
                Some(s) => proper_grammar.terminal_name(s),
                None => "$",
            };
            print!("{name}, ");
        }
        println!("}}");
    }

    let (item_sets, gotos) = proper_grammar.item_sets();

    println!();
    println!("################################################################################");
    println!("#                                   Item sets                                  #");
    println!("################################################################################");

    for (i, set) in item_sets.into_iter().enumerate() {
        println!("{i}:");
        for item in set.into_iter() {
            print!("    ");
            match item {
                grammar::Item::Start => println!("S' -> . start"),

                grammar::Item::End => println!("S' -> start ."),

                grammar::Item::Rule((head, body), dot) => {
                    print!("{} ->", proper_grammar.nonterminal_name(*head));
                    for i in 0..dot {
                        print!(" {}", proper_grammar.symbol_name(body[i]));
                    }
                    print!(" .");
                    for i in dot..body.len() {
                        print!(" {}", proper_grammar.symbol_name(body[i]));
                    }
                    println!();
                }
            }
        }
    }

    println!();
    println!("################################################################################");
    println!("#                                     GOTOs                                    #");
    println!("################################################################################");

    for ((from, symbol), to) in gotos {
        let name = proper_grammar.symbol_name(symbol);
        println!("GOTO({from}, {name}) = {to}");
    }

    proper_grammar
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
