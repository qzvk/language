#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Library for validating grammars and generating parse tables.

/// A context-free grammar.
pub struct Grammar<N, T> {
    start: Symbol<N, T>,
    rules: Vec<(N, Vec<Symbol<N, T>>)>,
}

impl<N, T> Grammar<N, T> {
    /// Create a new context free grammar with terminals `T`, nonterminals `N`, start symbol
    /// `start` and no rules.
    pub const fn new(start: Symbol<N, T>) -> Self {
        let rules = Vec::new();
        Self { start, rules }
    }

    /// The grammar's start symbol.
    pub const fn start(&self) -> &Symbol<N, T> {
        &self.start
    }

    /// Add a rewrite rule to the grammar `left -> right`. Duplicate rules are ignored.
    pub fn add_rule(&mut self, left: N, right: impl Into<Vec<Symbol<N, T>>>)
    where
        N: Ord,
        T: Ord,
    {
        let rule = (left, right.into());

        if let Err(index) = self.rules.binary_search(&rule) {
            self.rules.insert(index, rule);
        }
    }

    /// A sorted, unique list of the grammar's rewrite rules.
    pub fn rules(&self) -> impl Iterator<Item = &(N, Vec<Symbol<N, T>>)> {
        self.rules.iter()
    }
}

/// A grammar symbol. Either a nonterminal or terminal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<N, T> {
    /// A nonterminal symbol.
    Nonterminal(N),

    /// A terminal symbol.
    Terminal(T),
}

#[cfg(test)]
mod tests {
    use super::{Grammar, Symbol};

    #[test]
    fn can_create_grammar() {
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
        enum Nonterminal {
            A,
        }
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
        enum Terminal {
            B,
            C,
        }

        const START: Symbol<Nonterminal, Terminal> = Symbol::Nonterminal(Nonterminal::A);

        let mut grammar = Grammar::new(START);
        grammar.add_rule(Nonterminal::A, [Symbol::Terminal(Terminal::C)]); // Should be sorted after A -> B.
        grammar.add_rule(Nonterminal::A, [Symbol::Terminal(Terminal::B)]);
        grammar.add_rule(Nonterminal::A, [Symbol::Terminal(Terminal::B)]); // Duplicates should be discarded.

        let start = grammar.start();
        assert_eq!(&START, start);

        let rules: Vec<_> = grammar.rules().collect();
        assert_eq!(
            vec![
                &(Nonterminal::A, vec![Symbol::Terminal(Terminal::B)]),
                &(Nonterminal::A, vec![Symbol::Terminal(Terminal::C)])
            ],
            rules
        );
    }
}
