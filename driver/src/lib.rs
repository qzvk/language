#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Driver crate which orchestrates parsing and interpretation.

use std::{io::Read, process::ExitCode};

/// Run the interpreter from a file at `path`
pub fn run_file(path: String, verbose: bool) -> ExitCode {
    match std::fs::read_to_string(&path) {
        Ok(input) => run_string(input, verbose),

        Err(error) => {
            eprintln!("error: failed to read {path:?}: {error}");
            ExitCode::FAILURE
        }
    }
}

/// Run the interpreter from the standard input
pub fn run_stdin(verbose: bool) -> ExitCode {
    let mut stdin = std::io::stdin().lock();
    let mut input = String::new();

    match stdin.read_to_string(&mut input) {
        Ok(_) => run_string(input, verbose),
        Err(error) => {
            eprintln!("error: failed to read stdin: {error}");
            ExitCode::FAILURE
        }
    }
}

fn run_string(input: String, _verbose: bool) -> ExitCode {
    let tokens = lexer::lex(&input);

    match parser::parse(tokens) {
        Ok(ast) => {
            println!("{ast:?}");

            ExitCode::SUCCESS
        }

        Err(errors) => {
            eprintln!("{errors}");
            ExitCode::FAILURE
        }
    }
}
