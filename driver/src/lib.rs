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

fn run_string(input: String, _verbose: bool) -> Result<(), Error> {
    let tokens = lexer::lex(&input);

    // TODO: This is a workaround for the fact that we can't return parse::Error, since it keeps a
    // reference to `input`. Errors should be handled in this function, and a status returned to
    // the caller.
    let ast = parser::parse(tokens).map_err(|e| Error::Parse(e.to_string()))?;

    println!("{ast:?}");

    Ok(())
}

/// An error encountered during execution
#[derive(Debug)]
pub enum Error {
    /// Failed to read the source file
    ReadFile(String, std::io::Error),

    /// Failed to read standard input
    ReadStdin(std::io::Error),

    /// Failed to parse the input.
    Parse(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ReadFile(name, inner) => write!(f, "error: failed to read {name:?}: {inner}"),
            Error::ReadStdin(inner) => write!(f, "error: failed to read stdin: {inner}"),
            Error::Parse(inner) => inner.fmt(f),
        }
    }
}

impl std::error::Error for Error {}
