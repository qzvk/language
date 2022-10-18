#![forbid(unsafe_code)]

pub struct Grammar<N, T> {
    start: Symbol<N, T>,
}

impl<N, T> Grammar<N, T> {
    pub const fn new(start: Symbol<N, T>) -> Self {
        Self { start }
    }

    pub const fn start(&self) -> &Symbol<N, T> {
        &self.start
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Symbol<N, T> {
    Nonterminal(N),
    Terminal(T),
}

#[cfg(test)]
mod tests {
    use super::{Grammar, Symbol};

    #[test]
    fn can_create_grammar() {
        #[derive(Debug, PartialEq)]
        enum Nonterminal {
            A,
        }
        #[derive(Debug, PartialEq)]
        enum Terminal {}

        const START: Symbol<Nonterminal, Terminal> = Symbol::Nonterminal(Nonterminal::A);

        let grammar = Grammar::new(START);
        let start = grammar.start();
        assert_eq!(&START, start);
    }
}
