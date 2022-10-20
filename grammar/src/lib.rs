#![forbid(unsafe_code)]

pub struct Grammar<N, T> {
    start: Symbol<N, T>,
    rules: Vec<(N, Vec<Symbol<N, T>>)>,
}

impl<N, T> Grammar<N, T> {
    pub const fn new(start: Symbol<N, T>) -> Self {
        let rules = Vec::new();
        Self { start, rules }
    }

    pub const fn start(&self) -> &Symbol<N, T> {
        &self.start
    }

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

    pub fn rules(&self) -> impl Iterator<Item = &(N, Vec<Symbol<N, T>>)> {
        self.rules.iter()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol<N, T> {
    Nonterminal(N),
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
