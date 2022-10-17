use std::io::{stdin, Read};

mod lexer;

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    for (result, info) in lexer::lex(&input) {
        match result {
            Ok(kind) => println!("{info} {kind}"),
            Err(error) => println!("{info} {error}"),
        }
    }
}
