#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Driver crate which orchestrates parsing and interpretation.

use std::io::Read;

/// Run the interpreter from a file at `path`
pub fn run_file(path: String, verbose: bool) -> Result<(), Error> {
    let input = std::fs::read_to_string(&path).map_err(|e| Error::ReadFile(path, e))?;

    run_string(input, verbose)
}

/// Run the interpreter from the standard input
pub fn run_stdin(verbose: bool) -> Result<(), Error> {
    let mut stdin = std::io::stdin().lock();
    let mut input = String::new();
    stdin.read_to_string(&mut input).map_err(Error::ReadStdin)?;

    run_string(input, verbose)
}

fn run_string(input: String, verbose: bool) -> Result<(), Error> {
    if verbose {
        println!("Input: {input:?}");
    }

    for (kind, span) in lexer::lex(&input) {
        let line = span.line() + 1;
        let column = span.column() + 1;
        let source = span.source();
        println!("{line}:{column} {source} {kind}");
    }

    Ok(())
}

/// An error encountered during execution
#[derive(Debug)]
pub enum Error {
    /// Failed to read the source file
    ReadFile(String, std::io::Error),

    /// Failed to read standard input
    ReadStdin(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ReadFile(name, inner) => write!(f, "error: failed to read {name:?}: {inner}"),
            Error::ReadStdin(inner) => write!(f, "error: failed to read stdin: {inner}"),
        }
    }
}

impl std::error::Error for Error {}
