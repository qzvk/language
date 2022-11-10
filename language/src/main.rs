#![forbid(unsafe_code)]

use grammar::{Grammar, Nonterminal as _, ParseTable, ParseTree, Symbol, Terminal as _};
use lexer::TokenKind;
use std::io::{stdin, Read};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Nonterminal {
    Start,
    AssignmentSeq,
    Assignment,
    AddExpr,
    MulExpr,
    ApplyExpr,
    UnaryExpr,
    AddOp,
    MulOp,
}

impl grammar::Nonterminal for Nonterminal {
    type Iterator = std::array::IntoIter<Self, 9>;

    const COUNT: usize = 9;

    fn all() -> Self::Iterator {
        [
            Self::Start,
            Self::AssignmentSeq,
            Self::Assignment,
            Self::AddExpr,
            Self::MulExpr,
            Self::ApplyExpr,
            Self::UnaryExpr,
            Self::AddOp,
            Self::MulOp,
        ]
        .into_iter()
    }

    fn index(self) -> usize {
        self as usize
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::Start,
            1 => Self::AssignmentSeq,
            2 => Self::Assignment,
            3 => Self::AddExpr,
            4 => Self::MulExpr,
            5 => Self::ApplyExpr,
            6 => Self::UnaryExpr,
            7 => Self::AddOp,
            8 => Self::MulOp,
            _ => panic!(),
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            Self::Start => "start",
            Self::AssignmentSeq => "assignment-seq",
            Self::Assignment => "assignment",
            Self::AddExpr => "add-expr",
            Self::MulExpr => "mul-expr",
            Self::ApplyExpr => "apply-expr",
            Self::UnaryExpr => "unary-expr",
            Self::AddOp => "add-op",
            Self::MulOp => "mul-op",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Terminal(TokenKind);

impl grammar::Terminal for Terminal {
    type Iterator = std::array::IntoIter<Self, 10>;

    const COUNT: usize = 10;

    fn all() -> Self::Iterator {
        [
            Self(TokenKind::Plus),
            Self(TokenKind::Minus),
            Self(TokenKind::Asterisk),
            Self(TokenKind::Slash),
            Self(TokenKind::Semicolon),
            Self(TokenKind::Equals),
            Self(TokenKind::OpenParen),
            Self(TokenKind::CloseParen),
            Self(TokenKind::Integer),
            Self(TokenKind::Ident),
        ]
        .into_iter()
    }

    fn index(self) -> usize {
        self.0 as usize
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Self(TokenKind::Plus),
            1 => Self(TokenKind::Minus),
            2 => Self(TokenKind::Asterisk),
            3 => Self(TokenKind::Slash),
            4 => Self(TokenKind::Semicolon),
            5 => Self(TokenKind::Equals),
            6 => Self(TokenKind::OpenParen),
            7 => Self(TokenKind::CloseParen),
            8 => Self(TokenKind::Integer),
            9 => Self(TokenKind::Ident),
            _ => panic!(),
        }
    }

    fn as_str(&self) -> &'static str {
        match self.0 {
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Asterisk => "'*'",
            TokenKind::Slash => "'/'",
            TokenKind::Semicolon => "';'",
            TokenKind::Equals => "'='",
            TokenKind::OpenParen => "'('",
            TokenKind::CloseParen => "')'",
            TokenKind::Integer => "integer",
            TokenKind::Ident => "ident",
        }
    }
}

fn generate_parse_table(verbose: bool) -> ParseTable<Nonterminal, Terminal> {
    use Nonterminal::*;
    use Terminal as T;
    use TokenKind::*;

    let mut grammar = Grammar::new(Start);

    grammar.add_rule(Start).nonterminal(AssignmentSeq);

    grammar.add_rule(AssignmentSeq).nonterminal(Assignment);
    grammar
        .add_rule(AssignmentSeq)
        .nonterminal(Assignment)
        .terminal(T(Semicolon))
        .nonterminal(AssignmentSeq);

    grammar
        .add_rule(Assignment)
        .nonterminal(ApplyExpr)
        .terminal(T(Equals))
        .nonterminal(AddExpr);
    grammar.add_rule(Assignment).nonterminal(AddExpr);

    grammar
        .add_rule(AddExpr)
        .nonterminal(AddExpr)
        .nonterminal(AddOp)
        .nonterminal(MulExpr);
    grammar.add_rule(AddExpr).nonterminal(MulExpr);

    grammar
        .add_rule(MulExpr)
        .nonterminal(MulExpr)
        .nonterminal(MulOp)
        .nonterminal(ApplyExpr);
    grammar.add_rule(MulExpr).nonterminal(ApplyExpr);

    grammar
        .add_rule(ApplyExpr)
        .nonterminal(ApplyExpr)
        .nonterminal(UnaryExpr);
    grammar.add_rule(ApplyExpr).nonterminal(UnaryExpr);

    grammar
        .add_rule(UnaryExpr)
        .terminal(T(OpenParen))
        .nonterminal(AddExpr)
        .terminal(T(CloseParen));
    grammar.add_rule(UnaryExpr).terminal(T(Integer));
    grammar.add_rule(UnaryExpr).terminal(T(Ident));

    grammar.add_rule(AddOp).terminal(T(Plus));
    grammar.add_rule(AddOp).terminal(T(Minus));

    grammar.add_rule(MulOp).terminal(T(Asterisk));
    grammar.add_rule(MulOp).terminal(T(Slash));

    if verbose {
        println!("######################################################################");
        println!("#                               Grammar                              #");
        println!("######################################################################");
        print!("{}", grammar);
    }
    let proper_grammar = grammar.validate().unwrap();

    if verbose {
        println!();
        println!("######################################################################");
        println!("#                               Firsts                               #");
        println!("######################################################################");
        for symbol in Terminal::all()
            .into_iter()
            .map(Symbol::Terminal)
            .chain(Nonterminal::all().map(Symbol::Nonterminal))
        {
            print!("FIRST({}) = {{ ", symbol.as_str());

            for t in proper_grammar.first(&[symbol]) {
                print!("{}, ", t.as_str());
            }
            println!("}}");
        }

        println!();
        println!("######################################################################");
        println!("#                               Follows                              #");
        println!("######################################################################");

        for n in Nonterminal::all() {
            print!("FOLLOW({}) = {{ ", n.as_str());

            for t in proper_grammar.follow(n) {
                let name = match t {
                    Some(s) => s.as_str(),
                    None => "$",
                };
                print!("{name}, ");
            }
            println!("}}");
        }
    }

    let (item_sets, gotos) = proper_grammar.item_sets();

    if verbose {
        println!();
        println!("######################################################################");
        println!("#                              Item sets                             #");
        println!("######################################################################");

        for (i, set) in item_sets.into_iter().enumerate() {
            println!("{i}:");
            for item in set.into_iter() {
                print!("    ");
                match item {
                    grammar::Item::Start => println!("S' -> . start"),

                    grammar::Item::End => println!("S' -> start ."),

                    grammar::Item::Rule((head, body), dot) => {
                        print!("{} ->", head.as_str());
                        for i in 0..dot {
                            print!(" {}", body[i].as_str());
                        }
                        print!(" .");
                        for i in dot..body.len() {
                            print!(" {}", body[i].as_str());
                        }
                        println!();
                    }
                }
            }
        }

        println!();
        println!(
            "################################################################################"
        );
        println!(
            "#                                     GOTOs                                    #"
        );
        println!(
            "################################################################################"
        );

        for ((from, symbol), to) in gotos {
            let name = symbol.as_str();
            println!("GOTO({from}, {name}) = {to}");
        }
    }

    proper_grammar.parse_table().unwrap()
}

fn print_parse_tree(indent: usize, tree: ParseTree<Nonterminal, Terminal>) {
    match tree {
        ParseTree::Terminal(terminal) => {
            for _ in 0..indent {
                print!("    ");
            }
            println!("{}", terminal.as_str());
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
        println!("language 0.1.0");
        println!("Small experimental pure-functional language interpreter.");
        println!();
        println!("USAGE");
        println!("    language OPTIONS?");
        println!();
        println!("OPTIONS");
        println!("    --help      Display this help text.");
        println!("    --verbose   Print grammar information during parse table generation.");
        return;
    }

    let table = generate_parse_table(verbose);

    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let mut lex_errors = Vec::new();
    let tokens = lexer::lex(&input).filter_map(|(result, info)| match result {
        Ok(token) => Some(Terminal(token)),
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

        Err(error) => println!("{error}"),
    }
}
