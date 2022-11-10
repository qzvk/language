#![warn(missing_docs)]
#![forbid(unsafe_code)]

//! Root crate for the project, containing definitions and structures common between crates.

mod nonterminal;
mod parse_table;
mod token_info;
mod token_kind;

pub use crate::{
    nonterminal::Nonterminal, parse_table::parse_table, token_info::TokenInfo,
    token_kind::TokenKind,
};
