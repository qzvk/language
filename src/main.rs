use std::io::{stdin, Read};

mod lexer;

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();
    for (result, info) in lexer::lex(&input) {
        if let Ok(kind) = result {
            println!("{info} {kind}")
        }
    }
}
