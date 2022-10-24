#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Library for validating grammars and generating parse tables.

mod cycle;
use cycle::find_cycles;

/// A context-free grammar.
#[derive(Debug)]
pub struct Grammar {
    nonterminals: Vec<&'static str>,
    terminals: Vec<&'static str>,
    rules: Vec<(Nonterminal, Vec<Symbol>)>,
}

impl Grammar {
    /// Create a new grammar, returning its start nonterminal and itself.
    pub fn new() -> (Nonterminal, Self) {
        let start = Nonterminal(0);

        let grammar = Self {
            nonterminals: vec!["start"],
            terminals: Vec::new(),
            rules: Vec::new(),
        };

        (start, grammar)
    }

    /// Add a new, unique nonterminal to the grammar.
    pub fn add_nonterminal(&mut self, name: &'static str) -> Nonterminal {
        let index = self.nonterminals.len();
        self.nonterminals.push(name);
        Nonterminal(index as u32)
    }

    /// Add a new, unique terminal to the grammar.
    pub fn add_terminal(&mut self, name: &'static str) -> Terminal {
        let index = self.terminals.len();
        self.terminals.push(name);
        Terminal(index as u32)
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
    ///
    /// A proper grammar contains no:
    /// - unreachable symbols, which cannot be derived from the start symbol;
    /// - unproductive symbols, which cannot derive a string of terminals;
    /// - epsilon productions, which have an empty right-hand side;
    /// - cycles, where nonterminals can derive themselves.
    ///
    /// TODO: Currently, this does not modify the grammar. If the given grammar is not proper, this
    /// function fails. In the future, it might be interesting to transform the given grammar into
    /// a structurally equivalent one.
    pub fn validate(self) -> Result<ProperGrammar, Error> {
        self.validate_productivity()?;
        self.validate_reachability()?;
        self.validate_epsilon_productions()?;
        self.validate_acyclic()?;

        let nonterminal_first = self.calculate_firsts();

        Ok(ProperGrammar { nonterminal_first })
    }

    /// Validate that the grammar contains no unproductive nonterminals. See [`Self::validate`].
    fn validate_productivity(&self) -> Result<(), Error> {
        let mut productive = vec![false; self.nonterminals.len()];

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

        if unproductive_nonterminals.is_empty() {
            Ok(())
        } else {
            Err(Error::UnproductiveNonterminals(unproductive_nonterminals))
        }
    }

    /// Validate that the grammar contains no unreachable symbols. See [`Self::validate`].
    fn validate_reachability(&self) -> Result<(), Error> {
        let mut reachable_nonterminal = vec![false; self.nonterminals.len()];
        let mut reachable_terminal = vec![false; self.terminals.len()];

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

        if nonterminals.is_empty() && terminals.is_empty() {
            Ok(())
        } else {
            Err(Error::UnreachableSymbols(nonterminals, terminals))
        }
    }

    /// Validate that the grammar contains no epsilon productions. See [`Self::validate`].
    fn validate_epsilon_productions(&self) -> Result<(), Error> {
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

        if nonterminals.is_empty() {
            Ok(())
        } else {
            Err(Error::EpsilonProductions(nonterminals))
        }
    }

    /// Validate that the grammar contains no derivation cycles. See [`Self::validate`].
    fn validate_acyclic(&self) -> Result<(), Error> {
        let is_neighbour = |a: Nonterminal, b: Nonterminal| {
            // TODO: Since this will allocate for EVERY is_neighbour call, this is really slow.
            // Consider writing something faster.
            let value = (a, vec![Symbol::Nonterminal(b)]);
            self.rules.binary_search(&value).is_ok()
        };

        let cycles = find_cycles(self.nonterminals.len(), is_neighbour);

        if cycles.is_empty() {
            Ok(())
        } else {
            Err(Error::ContainsCycles(cycles))
        }
    }

    fn calculate_firsts(&self) -> Vec<Vec<Terminal>> {
        let mut sets = vec![Vec::new(); self.nonterminals.len()];

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                // This assumes that no symbols are nullable, and therefore only the first symbol
                // of a rewrite rule has to be checked.
                match right.get(0) {
                    Some(&Symbol::Nonterminal(n)) => {
                        let current_items = &sets[left.0 as usize];
                        let mut to_add = Vec::new();

                        for &s in &sets[n.0 as usize] {
                            if !current_items.contains(&s) {
                                to_add.push(s);
                                changed = true;
                            }
                        }

                        sets[left.0 as usize].extend(to_add);
                    }
                    Some(&Symbol::Terminal(t)) => {
                        let set = &mut sets[left.0 as usize];
                        if !set.contains(&t) {
                            set.push(t);
                            changed = true;
                        }
                    }
                    None => {
                        // This function should only be called after validation, before conversion
                        // to a ProperGrammar.
                        panic!("Epsilon production encountered during FIRST calculation.")
                    }
                }
            }

            if !changed {
                break;
            }
        }

        for set in &mut sets {
            set.sort();
        }

        sets
    }
}

impl std::fmt::Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (left, right) in &self.rules {
            write!(f, "{} ->", self.nonterminals[left.0 as usize])?;

            for r in right {
                let string = match r {
                    Symbol::Nonterminal(n) => self.nonterminals[n.0 as usize],
                    Symbol::Terminal(t) => self.terminals[t.0 as usize],
                };
                write!(f, " {}", string)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

/// A rewrite rule of a context-free grammar.
pub struct GrammarRule<'a> {
    rule: &'a mut (Nonterminal, Vec<Symbol>),
}

impl<'a> GrammarRule<'a> {
    /// Append a terminal to the rule.
    pub fn terminal(self, t: Terminal) -> Self {
        self.symbol(Symbol::Terminal(t))
    }

    /// Append a nonterminal to the rule.
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

impl cycle::Node for Nonterminal {
    fn from_usize(n: usize) -> Self {
        Self(n as u32)
    }
}

/// A terminal symbol of a grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Terminal(u32);

/// A proper context-free grammar. See [`Grammar::validate`].
#[derive(Debug)]
pub struct ProperGrammar {
    // TODO: Consider using a hash set instead?
    nonterminal_first: Vec<Vec<Terminal>>,
}

impl ProperGrammar {
    /// Return the set of symbols which may begin strings derived from `symbols.`
    ///
    /// TODO: Consider returning a slice of precomputed values.
    pub fn first(&self, symbols: &[Symbol]) -> Vec<Terminal> {
        match symbols.get(0) {
            Some(&Symbol::Nonterminal(n)) => self.nonterminal_first[n.0 as usize].clone(),
            Some(&Symbol::Terminal(t)) => vec![t],
            None => Vec::new(),
        }
    }
}

/// An error in a non-proper grammar.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// One or more nonterminals can never produce a string of terminals during derivation.
    UnproductiveNonterminals(Vec<Nonterminal>),

    /// One or more symbols are can not be produced by a derivation from the start symbol.
    UnreachableSymbols(Vec<Nonterminal>, Vec<Terminal>),

    /// One or more nonterminals can derive empty strings.
    EpsilonProductions(Vec<Nonterminal>),

    /// One or more nonterminals can derive themselves exactly.
    ContainsCycles(Vec<Vec<Nonterminal>>),
}

/// A symbol of a grammar, either terminal or nonterminal.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol {
    /// A nonterminal symbol.
    Nonterminal(Nonterminal),

    /// A terminal symbol.
    Terminal(Terminal),
}

#[cfg(test)]
mod tests {
    use super::{Error, Grammar, Nonterminal, ProperGrammar, Symbol, Terminal};

    #[test]
    fn can_create_grammar() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();

        let a: Nonterminal = grammar.add_nonterminal("A");
        assert_ne!(start, a);

        let b: Nonterminal = grammar.add_nonterminal("B");
        assert_ne!(start, b);
        assert_ne!(a, b);

        let c: Terminal = grammar.add_terminal("c");
        let d: Terminal = grammar.add_terminal("d");
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

        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let a = grammar.add_terminal("a");

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

        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        let c = grammar.add_terminal("c");

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

        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let z = grammar.add_nonterminal("Z");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        let c = grammar.add_terminal("c");

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

    #[test]
    fn cycles_are_not_proper() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();
        // S -> X
        // S -> Y
        // S -> U
        // X -> a
        // X -> X a
        // X -> b W
        // Y -> Z
        // Z -> Y a
        // Z -> W
        // W -> Y
        // W -> c
        // U -> V
        // V -> U
        // U -> b

        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let z = grammar.add_nonterminal("Z");
        let w = grammar.add_nonterminal("W");
        let u = grammar.add_nonterminal("U");
        let v = grammar.add_nonterminal("V");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        let c = grammar.add_terminal("c");

        grammar.add_rule(start).nonterminal(x);
        grammar.add_rule(start).nonterminal(y);
        grammar.add_rule(start).nonterminal(u);
        grammar.add_rule(x).terminal(a);
        grammar.add_rule(x).nonterminal(x).terminal(a);
        grammar.add_rule(x).terminal(b).nonterminal(w);
        grammar.add_rule(y).nonterminal(z);
        grammar.add_rule(z).nonterminal(y).terminal(a);
        grammar.add_rule(z).nonterminal(w);
        grammar.add_rule(w).nonterminal(y);
        grammar.add_rule(w).terminal(c);
        grammar.add_rule(u).nonterminal(v);
        grammar.add_rule(u).terminal(b);
        grammar.add_rule(v).nonterminal(u);

        let error: Error = grammar.validate().unwrap_err();
        assert_eq!(
            Error::ContainsCycles(vec![vec![y, z, w], vec![u, v]]),
            error
        );
    }

    #[test]
    fn can_display_grammar() {
        let (start, mut grammar): (Nonterminal, Grammar) = Grammar::new();

        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let c = grammar.add_terminal("c");
        let d = grammar.add_terminal("d");

        grammar.add_rule(start).nonterminal(x);
        grammar.add_rule(x).terminal(c);
        grammar.add_rule(y).terminal(d);
        grammar.add_rule(x).terminal(d).nonterminal(y).terminal(c);

        const EXPECTED: &str = "start -> X\nX -> c\nY -> d\nX -> d Y c\n";
        let actual = grammar.to_string();
        assert_eq!(EXPECTED, actual);
    }

    #[test]
    fn can_compute_first_sets_of_terminal_only_grammar() {
        let (start, mut grammar) = Grammar::new();
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        let c = grammar.add_terminal("c");
        grammar.add_rule(start).terminal(a);
        grammar.add_rule(start).terminal(b);
        grammar.add_rule(start).terminal(c);

        let grammar = grammar.validate().unwrap();
        let first_start = grammar.first(&[Symbol::Nonterminal(start)]);
        assert_eq!(vec![a, b, c], first_start);
    }

    #[test]
    fn can_compute_first_sets_of_example_grammar() {
        let (start, mut grammar) = Grammar::new();
        // S -> X
        // S -> Y
        // X -> a X
        // X -> b X
        // X -> d
        // Y -> Y Z
        // Y -> Z e
        // Z -> c Z
        // Z -> d

        let x = grammar.add_nonterminal("x");
        let y = grammar.add_nonterminal("y");
        let z = grammar.add_nonterminal("z");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        let c = grammar.add_terminal("c");
        let d = grammar.add_terminal("d");
        let e = grammar.add_terminal("d");
        grammar.add_rule(start).nonterminal(x);
        grammar.add_rule(start).nonterminal(y);
        grammar.add_rule(x).terminal(a).nonterminal(x);
        grammar.add_rule(x).terminal(b).nonterminal(x);
        grammar.add_rule(x).terminal(d);
        grammar.add_rule(y).nonterminal(y).nonterminal(z);
        grammar.add_rule(y).nonterminal(z).terminal(e);
        grammar.add_rule(z).terminal(c).nonterminal(z);
        grammar.add_rule(z).terminal(d);

        // I've left out tests for FIRST of more than 1 symbols. Since this operation is over a
        // proper grammar, where nullable symbols are not allowed, the FIRST algorithm never needs
        // to check more than 1 symbol from its input.

        let grammar = grammar.validate().unwrap();
        let first_empty = grammar.first(&[]);
        let first_start = grammar.first(&[Symbol::Nonterminal(start)]);
        let first_a = grammar.first(&[Symbol::Terminal(a)]);
        let first_b = grammar.first(&[Symbol::Terminal(b)]);
        let first_c = grammar.first(&[Symbol::Terminal(c)]);
        let first_d = grammar.first(&[Symbol::Terminal(d)]);
        let first_e = grammar.first(&[Symbol::Terminal(e)]);
        let first_x = grammar.first(&[Symbol::Nonterminal(x)]);
        let first_y = grammar.first(&[Symbol::Nonterminal(y)]);
        let first_z = grammar.first(&[Symbol::Nonterminal(z)]);

        assert!(first_empty.is_empty());
        assert_eq!(vec![a, b, c, d], first_start);
        assert_eq!(vec![a], first_a);
        assert_eq!(vec![b], first_b);
        assert_eq!(vec![c], first_c);
        assert_eq!(vec![d], first_d);
        assert_eq!(vec![e], first_e);
        assert_eq!(vec![a, b, d], first_x);
        assert_eq!(vec![c, d], first_y);
        assert_eq!(vec![c, d], first_z);
    }
}
