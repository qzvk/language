#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Parser crate for the interpreter. Converts token iterator into an abstract syntax tree.

mod parse_tree;

use lexer::Tokens;

/// An abstract syntax tree
pub struct Ast {}

/// An error encountered during parsing
pub enum Error {}

/// Parse a token stream into an abstract syntax tree
pub fn parse(_tokens: Tokens) -> Result<Ast, Error> {
    todo!()
}
