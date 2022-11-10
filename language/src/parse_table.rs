use crate::{Nonterminal, TokenKind};
use grammar::{Grammar, Nonterminal as _, ParseTable, Symbol, Terminal as _};

/// Generate the parse table for the language.
pub fn parse_table(verbose: bool) -> ParseTable<Nonterminal, TokenKind> {
    use Nonterminal::*;
    use TokenKind::*;

    let mut grammar = Grammar::new(Start);

    grammar.add_rule(Start).nonterminal(AssignmentSeq);

    grammar.add_rule(AssignmentSeq).nonterminal(Assignment);
    grammar
        .add_rule(AssignmentSeq)
        .nonterminal(Assignment)
        .terminal(Semicolon)
        .nonterminal(AssignmentSeq);

    grammar
        .add_rule(Assignment)
        .nonterminal(ApplyExpr)
        .terminal(Equals)
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
        .terminal(OpenParen)
        .nonterminal(AddExpr)
        .terminal(CloseParen);
    grammar.add_rule(UnaryExpr).terminal(Integer);
    grammar.add_rule(UnaryExpr).terminal(Ident);

    grammar.add_rule(AddOp).terminal(Plus);
    grammar.add_rule(AddOp).terminal(Minus);

    grammar.add_rule(MulOp).terminal(Asterisk);
    grammar.add_rule(MulOp).terminal(Slash);

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
        for symbol in TokenKind::all()
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
        println!("######################################################################");
        println!("#                                GOTOs                               #");
        println!("######################################################################");

        for ((from, symbol), to) in gotos {
            let name = symbol.as_str();
            println!("GOTO({from}, {name}) = {to}");
        }
    }

    proper_grammar.parse_table().unwrap()
}
