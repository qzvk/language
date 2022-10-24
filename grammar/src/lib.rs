#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Library for validating grammars and generating parse tables.

mod cycle;

/// A context-free grammar.
#[derive(Debug)]
pub struct Grammar {
    nonterminal_count: u32,
    terminal_count: u32,
    rules: Vec<(Nonterminal, Vec<Symbol>)>,
}

impl Grammar {
    /// Create a new grammar, returning its start nonterminal and itself.
    pub fn new() -> (Nonterminal, Self) {
        let start = Nonterminal(0);

        let grammar = Self {
            nonterminal_count: 1, // the start nonterminal is pre-defined
            terminal_count: 0,
            rules: Vec::new(),
        };

        (start, grammar)
    }

    /// Add a new, unique nonterminal to the grammar.
    pub fn add_nonterminal(&mut self) -> Nonterminal {
        let index = self.nonterminal_count;
        self.nonterminal_count += 1;
        Nonterminal(index)
    }

    /// Add a new, unique terminal to the grammar.
    pub fn add_terminal(&mut self) -> Terminal {
        let index = self.terminal_count;
        self.terminal_count += 1;
        Terminal(index)
    }

    /// Add a rule to the grammar.
    pub fn add_rule(&mut self, left: Nonterminal) -> GrammarRule {
        let rule = (left, Vec::new());
        self.rules.push(rule);

        // Safe to unwrap, we definitely have at least 1 element.
        let rule = self.rules.last_mut().unwrap();

        GrammarRule { rule }
    }

    /// The number of rules of the grammar.
    pub fn rule_count(&self) -> usize {
        self.rules.len()
    }

    /// If the grammar is proper, return `Ok`, otherwise return `Err`.
    pub fn validate(self) -> Result<ProperGrammar, Error> {
        let unproductive_nonterminals = self.unproductive_nonterminals();
        if !unproductive_nonterminals.is_empty() {
            return Err(Error::UnproductiveNonterminals(unproductive_nonterminals));
        }

        let (unreachable_nonterminals, unreachable_terminals) = self.unreachable_symbols();
        if !unreachable_nonterminals.is_empty() || !unreachable_terminals.is_empty() {
            return Err(Error::UnreachableSymbols(
                unreachable_nonterminals,
                unreachable_terminals,
            ));
        }

        let epsilon_productions = self.epsilon_productions();
        if !epsilon_productions.is_empty() {
            return Err(Error::EpsilonProductions(epsilon_productions));
        }

        Ok(ProperGrammar {})
    }

    fn unproductive_nonterminals(&self) -> Vec<Nonterminal> {
        let mut productive = vec![false; self.nonterminal_count as usize];

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                let mut is_definitely_productive = true;

                for r in right {
                    if let Symbol::Nonterminal(n) = r {
                        if !productive[n.0 as usize] {
                            is_definitely_productive = false;
                            break;
                        }
                    }
                }

                if is_definitely_productive && !productive[left.0 as usize] {
                    productive[left.0 as usize] = true;
                    changed = true;
                }
            }

            if !changed {
                break;
            }
        }

        let mut unproductive_nonterminals = Vec::new();
        for (p, is_productive) in productive.into_iter().enumerate() {
            if !is_productive {
                unproductive_nonterminals.push(Nonterminal(p as u32));
            }
        }

        unproductive_nonterminals
    }

    fn unreachable_symbols(&self) -> (Vec<Nonterminal>, Vec<Terminal>) {
        let mut reachable_nonterminal = vec![false; self.nonterminal_count as usize];
        let mut reachable_terminal = vec![false; self.terminal_count as usize];

        reachable_nonterminal[0] = true; // start nonterminal is reachable by definition

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                if !reachable_nonterminal[left.0 as usize] {
                    continue;
                }

                for r in right {
                    let reachability = match r {
                        Symbol::Nonterminal(n) => &mut reachable_nonterminal[n.0 as usize],
                        Symbol::Terminal(t) => &mut reachable_terminal[t.0 as usize],
                    };

                    if !(*reachability) {
                        *reachability = true;
                        changed = true;
                    }
                }
            }

            if !changed {
                break;
            }
        }

        let mut nonterminals = Vec::new();
        for (n, is_reachable) in reachable_nonterminal.into_iter().enumerate() {
            if !is_reachable {
                nonterminals.push(Nonterminal(n as u32));
            }
        }

        let mut terminals = Vec::new();
        for (t, is_reachable) in reachable_terminal.into_iter().enumerate() {
            if !is_reachable {
                terminals.push(Terminal(t as u32));
            }
        }

        (nonterminals, terminals)
    }

    fn epsilon_productions(&self) -> Vec<Nonterminal> {
        let mut nonterminals = Vec::new();

        for (left, right) in &self.rules {
            if !right.is_empty() {
                continue; // Not an epsilon rule.
            }

            // This method of insertion keeps `nonterminals` sorted.
            if let Err(index) = nonterminals.binary_search(left) {
                nonterminals.insert(index, *left);
            }
        }

        nonterminals
    }
}

/// A rewrite rule of a context-free grammar.
pub struct GrammarRule<'a> {
    rule: &'a mut (Nonterminal, Vec<Symbol>),
}

impl<'a> GrammarRule<'a> {
    /// Append a terminal to the of the rule.
    pub fn terminal(self, t: Terminal) -> Self {
        self.symbol(Symbol::Terminal(t))
    }

    /// Append a nonterminal to the of the rule.
    pub fn nonterminal(self, n: Nonterminal) -> Self {
        self.symbol(Symbol::Nonterminal(n))
    }

    fn symbol(self, s: Symbol) -> Self {
        self.rule.1.push(s);
        self
    }
}

/// A nonterminal symbol of a grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Nonterminal(u32);

/// A terminal symbol of a grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Terminal(u32);

/// A proper context-free grammar. See [`Grammar::validate`].
#[derive(Debug)]
pub struct ProperGrammar {}

/// An error in a non-proper grammar.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// One or more nonterminals can never produce a string of terminals during derivation.
    UnproductiveNonterminals(Vec<Nonterminal>),

    /// One or more symbols are can not be produced by a derivation from the start symbol.
    UnreachableSymbols(Vec<Nonterminal>, Vec<Terminal>),

    /// One or more nonterminals can derive empty strings.
    EpsilonProductions(Vec<Nonterminal>),
}

#[derive(Debug)]
enum Symbol {
    Nonterminal(Nonterminal),
    Terminal(Terminal),
}

#[cfg(test)]
mod tests {
    use super::{Error, Grammar, Nonterminal, ProperGrammar, Terminal};

    #[test]
    fn can_create_grammar() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();

        let a: Nonterminal = grammar.add_nonterminal();
        assert_ne!(start, a);

        let b: Nonterminal = grammar.add_nonterminal();
        assert_ne!(start, b);
        assert_ne!(a, b);

        let c: Terminal = grammar.add_terminal();
        let d: Terminal = grammar.add_terminal();
        assert_ne!(c, d);

        assert_eq!(0, grammar.rule_count());

        grammar.add_rule(a).terminal(c);
        assert_eq!(1, grammar.rule_count());

        grammar.add_rule(b).terminal(d);
        assert_eq!(2, grammar.rule_count());

        grammar.add_rule(a).terminal(d).nonterminal(b).terminal(c);
        assert_eq!(3, grammar.rule_count());

        grammar.add_rule(start).nonterminal(a);
        assert_eq!(4, grammar.rule_count());

        let _proper_grammar: ProperGrammar = grammar.validate().unwrap();
    }

    #[test]
    fn unproductive_grammar_is_not_proper() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
        // S -> X
        // X -> a
        // X -> Y

        let x = grammar.add_nonterminal();
        let y = grammar.add_nonterminal();
        let a = grammar.add_terminal();

        grammar.add_rule(start).nonterminal(x);
        grammar.add_rule(x).terminal(a);
        grammar.add_rule(x).nonterminal(y);

        let error: Error = grammar.validate().unwrap_err();
        assert_eq!(Error::UnproductiveNonterminals(vec![y]), error);
    }

    #[test]
    fn unreachable_grammar_is_not_proper() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
        // S -> X
        // X -> a
        // Y -> b
        // unreachable: b, Y, c

        let x = grammar.add_nonterminal();
        let y = grammar.add_nonterminal();
        let a = grammar.add_terminal();
        let b = grammar.add_terminal();
        let c = grammar.add_terminal();

        grammar.add_rule(start).nonterminal(x);
        grammar.add_rule(x).terminal(a);
        grammar.add_rule(y).terminal(b);

        let error: Error = grammar.validate().unwrap_err();
        assert_eq!(Error::UnreachableSymbols(vec![y], vec![b, c]), error);
    }

    #[test]
    fn epsilon_productions_are_not_proper() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
        // S -> X
        // S -> Y
        // S -> Z
        // X ->
        // X -> a X
        // Y -> b Z c
        // Z ->

        let x = grammar.add_nonterminal();
        let y = grammar.add_nonterminal();
        let z = grammar.add_nonterminal();
        let a = grammar.add_terminal();
        let b = grammar.add_terminal();
        let c = grammar.add_terminal();

        grammar.add_rule(start).nonterminal(x);
        grammar.add_rule(start).nonterminal(y);
        grammar.add_rule(start).nonterminal(z);
        grammar.add_rule(z);
        grammar.add_rule(x);
        grammar.add_rule(x).terminal(a).nonterminal(x);
        grammar.add_rule(y).terminal(b).nonterminal(z).terminal(c);

        let error: Error = grammar.validate().unwrap_err();
        assert_eq!(Error::EpsilonProductions(vec![x, z]), error);
    }
}
