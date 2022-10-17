use std::io::{stdin, Read};

mod lexer;

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    for (kind, info) in lexer::lex(&input) {
        println!("{info} {kind}");
    }
}
