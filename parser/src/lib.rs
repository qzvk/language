#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Parser crate for the interpreter. Converts token iterator into an abstract syntax tree.

mod parse_tree;

pub use parse_tree::Error as SyntaxError;

use crate::parse_tree::AssignmentSeq;
use lexer::Tokens;

/// An abstract syntax tree
pub struct Ast {}

/// An error encountered during parsing
#[derive(Debug)]
pub enum Error<'a> {
    /// One or more issues were found with the syntax of the input.
    Syntax(Vec<SyntaxError<'a>>),
}

impl<'a> From<Vec<SyntaxError<'a>>> for Error<'a> {
    fn from(errors: Vec<SyntaxError<'a>>) -> Self {
        Self::Syntax(errors)
    }
}

impl<'a> std::fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Syntax(errors) => {
                for error in errors {
                    error.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

/// Parse a token stream into an abstract syntax tree
pub fn parse(tokens: Tokens) -> Result<Ast, Error> {
    let mut tokens = tokens.peekable();
    let parse_tree = AssignmentSeq::parse(&mut tokens)?;

    println!("{parse_tree:#?}");

    todo!()
}
