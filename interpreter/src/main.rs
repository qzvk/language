#![forbid(unsafe_code)]

use grammar::{Nonterminal as _, ParseTree, Terminal as _};
use language::{Nonterminal, TokenInfo, TokenKind};
use std::io::{stdin, Read};

fn print_parse_tree(indent: usize, tree: ParseTree<Nonterminal, TokenKind, TokenInfo>) {
    match tree {
        ParseTree::Terminal(terminal, info) => {
            for _ in 0..indent {
                print!("    ");
            }
            println!("{} ({})", terminal.as_str(), info);
        }

        ParseTree::Nonterminal(nonterminal, children) => {
            for _ in 0..indent {
                print!("    ");
            }
            println!("({}", nonterminal.as_str());
            for child in children {
                print_parse_tree(indent + 1, child);
            }
            for _ in 0..indent {
                print!("    ");
            }
            println!(")");
        }
    }
}

fn main() {
    let help = std::env::args().any(|arg| arg == "--help");
    let verbose = std::env::args().any(|arg| arg == "--verbose");

    if help {
        println!("interpreter 0.1.0");
        println!("Small experimental pure-functional language interpreter.");
        println!();
        println!("USAGE");
        println!("    interpreter OPTIONS?");
        println!();
        println!("OPTIONS");
        println!("    --help      Display this help text.");
        println!("    --verbose   Print grammar information during parse table generation.");
        return;
    }

    let table = language::parse_table(verbose);

    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let mut lex_errors = Vec::new();
    let tokens = lexer::lex(&input).filter_map(|(result, info)| match result {
        Ok(token) => Some((token, info)),
        Err(error) => {
            lex_errors.push((error, info));
            None
        }
    });

    let parse_result = table.parse(tokens);

    if !lex_errors.is_empty() {
        for (error, info) in lex_errors {
            println!(
                "{}:{}: error: {}",
                info.line() + 1,
                info.column() + 1,
                error
            );
        }

        return;
    }

    match parse_result {
        Ok(tree) => print_parse_tree(0, tree),

        Err((error, Some(info))) => println!(
            "{}:{}: error: {}",
            info.line() + 1,
            info.column() + 1,
            error
        ),

        Err((error, None)) => println!("EOF: error: {}", error),
    }
}
