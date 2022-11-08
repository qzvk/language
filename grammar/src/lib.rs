#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Library for validating grammars and generating parse tables.

mod cycle;

use crate::cycle::find_cycles;
use std::collections::HashMap;

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

        let firsts = self.calculate_firsts();
        let follows = self.calculate_follows(&firsts);

        let mut rules = Vec::with_capacity(self.rules.len());
        for rule in self.rules {
            rules.push((rule.0, rule.1.into_boxed_slice()));
        }

        Ok(ProperGrammar {
            nonterminals: self.nonterminals,
            terminals: self.terminals,
            firsts,
            follows,
            rules: rules.into_boxed_slice(),
        })
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

    fn calculate_follows(&self, firsts: &[Vec<Terminal>]) -> Vec<Vec<Option<Terminal>>> {
        let mut follows = vec![Vec::new(); self.nonterminals.len()];

        // Add endmarker to start symbol.
        follows[0].push(None);

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                for window in right.windows(2) {
                    let (a, b) = (&window[0], &window[1]);

                    if let Symbol::Nonterminal(n) = a {
                        match b {
                            Symbol::Nonterminal(x) => {
                                for &y in &firsts[x.0 as usize] {
                                    if !follows[n.0 as usize].contains(&Some(y)) {
                                        follows[n.0 as usize].push(Some(y));
                                        changed = true;
                                    }
                                }
                            }
                            &Symbol::Terminal(t) => {
                                if !follows[n.0 as usize].contains(&Some(t)) {
                                    follows[n.0 as usize].push(Some(t));
                                    changed = true;
                                }
                            }
                        }
                    }
                }

                if let Some(Symbol::Nonterminal(n)) = right.last() {
                    let current_items = &follows[n.0 as usize];
                    let mut new_items = Vec::new();

                    for &item in &follows[left.0 as usize] {
                        if !current_items.contains(&item) {
                            new_items.push(item);
                            changed = true;
                        }
                    }

                    follows[n.0 as usize].extend(new_items);
                }
            }

            if !changed {
                break;
            }
        }

        follows
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Nonterminal(u32);

impl cycle::Node for Nonterminal {
    fn from_usize(n: usize) -> Self {
        Self(n as u32)
    }
}

/// A terminal symbol of a grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Terminal(u32);

/// A proper context-free grammar. See [`Grammar::validate`].
#[derive(Debug)]
pub struct ProperGrammar {
    nonterminals: Vec<&'static str>,
    terminals: Vec<&'static str>,
    // TODO: Consider using a hash set instead?
    firsts: Vec<Vec<Terminal>>,
    // TODO: Again, consider the HashSet.
    follows: Vec<Vec<Option<Terminal>>>,
    rules: Box<[(Nonterminal, Box<[Symbol]>)]>,
}

impl ProperGrammar {
    /// Return the set of terminals which may begin strings derived from `symbols.`
    ///
    /// TODO: Consider returning a slice of precomputed values.
    pub fn first(&self, symbols: &[Symbol]) -> Vec<Terminal> {
        match symbols.get(0) {
            Some(&Symbol::Nonterminal(n)) => self.firsts[n.0 as usize].clone(),
            Some(&Symbol::Terminal(t)) => vec![t],
            None => Vec::new(),
        }
    }

    /// Return the FOLLOW set of terminals which may follow `symbol` in a derivation. `None` is
    /// used to encode the endmarker.
    pub fn follow(&self, nonterminal: Nonterminal) -> Vec<Option<Terminal>> {
        self.follows[nonterminal.0 as usize].clone()
    }

    /// The name of the given nonterminal.
    pub fn nonterminal_name(&self, nonterminal: Nonterminal) -> &'static str {
        self.nonterminals[nonterminal.0 as usize]
    }

    /// The name of the given terminal.
    pub fn terminal_name(&self, terminal: Terminal) -> &'static str {
        self.terminals[terminal.0 as usize]
    }

    /// The name of the given terminal.
    pub fn symbol_name(&self, symbol: Symbol) -> &'static str {
        match symbol {
            Symbol::Nonterminal(n) => self.nonterminal_name(n),
            Symbol::Terminal(t) => self.terminal_name(t),
        }
    }

    /// Generate the item sets for this grammar. The GOTO state function is also provided.
    pub fn item_sets(&self) -> (Vec<ItemSet>, HashMap<(usize, Symbol), usize>) {
        let set_0 = self.closure(ItemSet::from(Item::Start));
        let mut collection = vec![set_0];
        let mut gotos = HashMap::new();

        let mut changed = true;
        while changed {
            changed = false;

            // TODO: Not efficient, optimize this at some point.
            let sets = collection.clone().into_iter().enumerate();
            for (i, set) in sets {
                for symbol in self.symbols() {
                    let goto_set = self.goto(&set, symbol);
                    if goto_set.is_empty() {
                        continue;
                    }

                    if let Some(index) = collection.iter().position(|s| s == &goto_set) {
                        // If we've seen this before, only add it as a GOTO.
                        gotos.insert((i, symbol), index);
                    } else {
                        // Otherwise, add a GOTO and push the set onto the collection.
                        let j = collection.len();
                        gotos.insert((i, symbol), j);

                        collection.push(goto_set);
                        changed = true;
                    }
                }
            }
        }

        (collection, gotos)
    }

    fn symbols(&self) -> impl Iterator<Item = Symbol> {
        let nonterminals = (0..self.nonterminals.len()).map(|index| {
            let symbol = Nonterminal(index as u32);
            Symbol::Nonterminal(symbol)
        });

        let terminals = (0..self.terminals.len()).map(|index| {
            let symbol = Terminal(index as u32);
            Symbol::Terminal(symbol)
        });

        nonterminals.chain(terminals)
    }

    /// Return the symbol after the dot for the given item.
    fn next_symbol(&self, item: Item) -> Option<Symbol> {
        match item {
            Item::Start => {
                const START: Nonterminal = Nonterminal(0);
                Some(Symbol::Nonterminal(START))
            }

            Item::End => None,

            Item::Rule((_, body), dot) => body.get(dot).cloned(),
        }
    }

    fn closure<'a>(&'a self, mut item_set: ItemSet<'a>) -> ItemSet {
        let mut changed = true;
        while changed {
            changed = false;

            // TODO: This is really inefficient, and should be optimized at some point.
            for item in item_set.items.clone() {
                if let Some(Symbol::Nonterminal(n)) = self.next_symbol(item) {
                    for rule in self.rules.iter() {
                        if rule.0 == n {
                            let new_item = Item::Rule(rule, 0);
                            changed |= item_set.push(new_item);
                        }
                    }
                }
            }
        }

        item_set
    }

    fn next_item<'a>(&'a self, item: Item<'a>) -> Option<Item> {
        match item {
            Item::Start => Some(Item::End),

            Item::End => None,

            Item::Rule(rule, dot) => {
                if rule.1.len() == dot {
                    None
                } else {
                    Some(Item::Rule(rule, dot + 1))
                }
            }
        }
    }

    fn goto<'a>(&'a self, item_set: &ItemSet<'a>, symbol: Symbol) -> ItemSet {
        let mut goto_set = ItemSet::new();

        for &item in &item_set.items {
            if let (Some(next), Some(s)) = (self.next_item(item), self.next_symbol(item)) {
                if symbol == s {
                    goto_set.push(next);
                }
            }
        }

        self.closure(goto_set)
    }

    /// Generate an SLR parse table for the grammar. (TODO: LALR parse table generation.)
    pub fn parse_table(&self) -> Result<ParseTable, ParseTableConflict> {
        let mut actions = HashMap::new();
        // TODO: Consider splitting GOTO into separate terminal and nonterminal lookups.
        let (items, gotos) = self.item_sets();

        // For each item set in the collection.
        for (i, set) in items.into_iter().enumerate() {
            // All else is error.

            for item in set {
                match item {
                    Item::Start => { /* no processing needed for S' -> . S */ }

                    // If S' -> S . in this set, action[i, $] is accept.
                    Item::End => {
                        let old = actions.insert((i, None), ParseAction::Accept);
                        match old {
                            Some(ParseAction::Accept) | None => {}
                            _ => panic!("Accept conflicts should not be possible, this is a bug."),
                        }
                    }

                    Item::Rule((head, body), dot) => {
                        if dot == body.len() {
                            // If A -> _ . in this set, then action[i, a] = reduce A -> _ for all a in FOLLOW(A).

                            for a in self.follow(*head) {
                                let action = ParseAction::Reduce(*head, body.len());
                                let old = actions.insert((i, a), action);
                                if old.is_some() {
                                    // Just overwrote a different action, this is a conflict.
                                    return Err(ParseTableConflict::ReduceReduce);
                                }
                            }
                        } else if let Symbol::Terminal(a) = body[dot] {
                            // If A -> _ . a _ in this set and GOTO(i, a) = j, then action[i, a] = shift j. a is a terminal

                            let j = gotos[&(i, Symbol::Terminal(a))];
                            let action = ParseAction::Shift(j);
                            let old = actions.insert((i, Some(a)), action);

                            // TODO: Move this stuff into ParseTableBuilder struct or something.
                            if old.is_some() && (old != Some(action)) {
                                // Just overwrote a different action, this is a conflict.
                                return Err(ParseTableConflict::ShiftReduce);
                            }
                        }
                    }
                }
            }
        }

        Ok(ParseTable {
            actions,
            gotos: Self::nonterminal_gotos(gotos),
        })
    }

    /// Remove nonterminals from a GOTO map.
    fn nonterminal_gotos(
        symbols: HashMap<(usize, Symbol), usize>,
    ) -> HashMap<(usize, Nonterminal), usize> {
        let mut nonterminals = HashMap::new();

        for ((state, symbol), next_state) in symbols {
            if let Symbol::Nonterminal(nonterminal) = symbol {
                nonterminals.insert((state, nonterminal), next_state);
            }
        }

        nonterminals
    }
}

/// An action performable by a shift-reduce parser.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseAction {
    /// Shift a token from the input, pushing this state onto the stack.
    Shift(usize),

    /// Reduce this many symbols into the nonterminal.
    Reduce(Nonterminal, usize),

    /// The syntax of the input is incorrect.
    Error,

    /// The input should be accepted.
    Accept,
}

/// A conflict found during parse table generation which prevents the production of an SLR parser.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseTableConflict {
    /// A conflict between a shift action and reduce action.
    ShiftReduce,

    /// A conflict between two reduce action and reduce action.
    ReduceReduce,
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    /// A nonterminal symbol.
    Nonterminal(Nonterminal),

    /// A terminal symbol.
    Terminal(Terminal),
}

/// A grammar item, representing a partially parsed production rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Item<'a> {
    /// The item `S' -> . S`.
    Start,

    /// The item `S' -> S .`
    End,

    /// A non-start item.
    Rule(&'a (Nonterminal, Box<[Symbol]>), usize),
}

/// A set of grammar items.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct ItemSet<'a> {
    // TODO: Consider not storing non-kernel items?
    // TODO: Consider using a HashSet?
    items: Vec<Item<'a>>,
}

impl<'a> ItemSet<'a> {
    /// Create a new, empty set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Whether the set contains zero elements.
    pub fn is_empty(&self) -> bool {
        self.items.len() == 0
    }

    /// The number of items the set contains.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Push an item into the set, returning whether it was newly inserted.
    pub fn push(&mut self, item: Item<'a>) -> bool {
        if let Err(index) = self.items.binary_search(&item) {
            self.items.insert(index, item);
            true
        } else {
            false
        }
    }

    /// Whether the set contains the given item.
    pub fn contains(&self, item: &Item) -> bool {
        self.items.binary_search(item).is_ok()
    }
}

impl<'a> IntoIterator for ItemSet<'a> {
    type Item = Item<'a>;

    type IntoIter = <Vec<Item<'a>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> From<Item<'a>> for ItemSet<'a> {
    fn from(item: Item<'a>) -> Self {
        Self { items: vec![item] }
    }
}

/// A parse table used for shift/reduce decisions during parsing.
#[derive(Debug)]
pub struct ParseTable {
    // TODO: Consider 2D arrays? Benchmark
    actions: HashMap<(usize, Option<Terminal>), ParseAction>,
    gotos: HashMap<(usize, Nonterminal), usize>,
}

impl ParseTable {
    /// The corresponding action for the given state and input. `None` represents the endmarker.
    pub fn action(&self, state: usize, input: Option<Terminal>) -> ParseAction {
        self.actions
            .get(&(state, input))
            .cloned()
            .unwrap_or(ParseAction::Error)
    }

    /// The state to transition to given an input and state.
    pub fn goto(&self, state: usize, input: Nonterminal) -> usize {
        if !self.gotos.contains_key(&(state, input)) {
            panic!("Bad goto for state {} and input {}", state, input.0);
        }
        self.gotos[&(state, input)]
    }

    /// Parse a sequence of terminals into a parse tree.
    pub fn parse(&self, mut input: impl Iterator<Item = Terminal>) -> ParseTree {
        let dummy_tree = ParseTree::Terminal(Terminal(0));

        let mut a = input.next();
        let mut stack = vec![(dummy_tree, 0)];

        loop {
            let (_, head_state) = stack.last().cloned().unwrap();

            match self.action(head_state, a) {
                ParseAction::Shift(t) => {
                    stack.push((ParseTree::Terminal(a.unwrap()), t));
                    a = input.next();
                }

                ParseAction::Reduce(a, len) => {
                    let mut subtrees = Vec::with_capacity(len);
                    let remove_index = stack.len() - len;
                    for _ in 0..len {
                        let (item, _) = stack.remove(remove_index);
                        subtrees.push(item);
                    }

                    let tree = ParseTree::Nonterminal(a, subtrees);

                    let (_, t) = stack.last().cloned().unwrap();

                    stack.push((tree, self.goto(t, a)));
                }

                ParseAction::Error => todo!("Handling errors during parsing is not implemented."),

                ParseAction::Accept => break,
            }
        }

        // Check that the stack contains the 'dummy' start state's tree, and the final result tree.
        assert_eq!(2, stack.len());

        stack.pop().unwrap().0
    }
}

/// A tree of symbols generated during parsing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseTree {
    /// A terminal node, corresponding to an input symbol.
    Terminal(Terminal),

    /// A nonterminal node, containing subtrees.
    Nonterminal(Nonterminal, Vec<ParseTree>),
}

#[cfg(test)]
mod tests {
    use super::{Grammar, Item, ItemSet, Nonterminal, Symbol, Terminal};

    #[test]
    fn proper_grammar_takes_grammar_rules() {
        let (s, mut grammar) = Grammar::new();
        // S -> X Y
        // S -> X
        // X -> c X b
        // X -> d
        // Y -> Y a
        // Y -> b

        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        let c = grammar.add_terminal("c");

        grammar.add_rule(s).nonterminal(x).nonterminal(y);
        grammar.add_rule(s).nonterminal(x);
        grammar.add_rule(x).terminal(c).nonterminal(x).terminal(b);
        grammar.add_rule(x).terminal(c);
        grammar.add_rule(y).nonterminal(y).terminal(a);
        grammar.add_rule(y).terminal(b);

        let grammar = grammar.validate().unwrap();

        use Symbol::Nonterminal as N;
        use Symbol::Terminal as T;

        assert_eq!(
            [
                (s, vec![N(x), N(y)].into_boxed_slice()),
                (s, vec![N(x)].into_boxed_slice()),
                (x, vec![T(c), N(x), T(b)].into_boxed_slice()),
                (x, vec![T(c)].into_boxed_slice()),
                (y, vec![N(y), T(a)].into_boxed_slice()),
                (y, vec![T(b)].into_boxed_slice()),
            ],
            grammar.rules.as_ref()
        );
    }

    #[test]
    fn can_get_subsequent_nonterminal_from_item() {
        let (s, mut grammar) = Grammar::new();
        let x = grammar.add_nonterminal("X");
        let a = grammar.add_terminal("a");
        grammar.add_rule(s).terminal(a).nonterminal(x);
        grammar.add_rule(x).terminal(a);
        let grammar = grammar.validate().unwrap();

        // S' -> . S ==> S
        let expected = grammar.next_symbol(Item::Start);
        assert_eq!(Some(Symbol::Nonterminal(s)), expected);

        // S' -> S' S . ==> (nothing)
        let expected = grammar.next_symbol(Item::End);
        assert_eq!(None, expected);

        // S -> a . X ==> X
        let expected = grammar.next_symbol(Item::Rule(&grammar.rules[0], 1));
        assert_eq!(Some(Symbol::Nonterminal(x)), expected);

        // X -> . a ==> a
        let expected = grammar.next_symbol(Item::Rule(&grammar.rules[1], 0));
        assert_eq!(Some(Symbol::Terminal(a)), expected);

        // X -> b . ==> (nothing)
        let expected = grammar.next_symbol(Item::Rule(&grammar.rules[1], 1));
        assert_eq!(None, expected);
    }

    #[test]
    fn can_push_to_item_set() {
        let mut set = ItemSet::new();
        assert_eq!(0, set.len());

        assert!(set.push(Item::Start));
        assert_eq!(1, set.len());

        assert!(!set.push(Item::Start));
        assert_eq!(1, set.len());

        assert!(set.push(Item::End));
        assert_eq!(2, set.len());

        let rule = (
            Nonterminal(0),
            vec![Symbol::Terminal(Terminal(0))].into_boxed_slice(),
        );

        assert!(set.push(Item::Rule(&rule, 0)));
        assert!(set.push(Item::Rule(&rule, 1)));
        assert_eq!(4, set.len());
    }

    #[test]
    fn can_take_closure_of_items() {
        // S -> a X
        // S -> Y a
        // X -> b Y
        // Y -> a

        let (s, mut grammar) = Grammar::new();
        let x = grammar.add_nonterminal("X");
        let y = grammar.add_nonterminal("Y");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        grammar.add_rule(s).terminal(a).nonterminal(x);
        grammar.add_rule(s).nonterminal(y).terminal(a);
        grammar.add_rule(x).terminal(b).nonterminal(y);
        grammar.add_rule(y).terminal(a);
        let grammar = grammar.validate().unwrap();

        // S' -> . S
        // S -> . a X
        // S -> . Y a
        // Y -> . a
        let expected = {
            let mut set = ItemSet::new();
            set.push(Item::Start);
            set.push(Item::Rule(&grammar.rules[0], 0));
            set.push(Item::Rule(&grammar.rules[1], 0));
            set.push(Item::Rule(&grammar.rules[3], 0));
            set
        };
        let start = ItemSet::from(Item::Start);
        let actual = grammar.closure(start);
        assert_eq!(expected, actual);

        // S -> a . X
        // X -> . b Y
        let expected = {
            let mut set = ItemSet::new();
            set.push(Item::Rule(&grammar.rules[0], 1));
            set.push(Item::Rule(&grammar.rules[2], 0));
            set
        };
        let start = ItemSet::from(Item::Rule(&grammar.rules[0], 1));
        let actual = grammar.closure(start);
        assert_eq!(expected, actual);
    }

    #[test]
    fn can_get_next_item() {
        // S' -> S
        // X -> a
        let (s, mut grammar) = Grammar::new();
        let a = grammar.add_terminal("a");
        grammar.add_rule(s).terminal(a);
        let grammar = grammar.validate().unwrap();

        // S' -> . S ==> S' -> S .
        assert_eq!(Some(Item::End), grammar.next_item(Item::Start));

        // S' -> S . ==> (nothing)
        assert_eq!(None, grammar.next_item(Item::End));

        // S -> . a ==> S -> a .
        assert_eq!(
            Some(Item::Rule(&grammar.rules[0], 1)),
            grammar.next_item(Item::Rule(&grammar.rules[0], 0))
        );

        // S -> a . ==> (nothing)
        assert_eq!(None, grammar.next_item(Item::Rule(&grammar.rules[0], 1)));
    }

    #[test]
    fn can_calculate_item_set_gotos() {
        // S' -> S
        // S -> a X
        // X -> b
        let (s, mut grammar) = Grammar::new();
        let x = grammar.add_nonterminal("X");
        let a = grammar.add_terminal("a");
        let b = grammar.add_terminal("b");
        grammar.add_rule(s).terminal(a).nonterminal(x);
        grammar.add_rule(x).terminal(b);
        let grammar = grammar.validate().unwrap();

        // GOTO({S' -> . S}, S) = {S' -> S .}
        assert_eq!(
            ItemSet::from(Item::End),
            grammar.goto(&ItemSet::from(Item::Start), Symbol::Nonterminal(s))
        );

        // GOTO({S' -> S .}, a) = { }
        assert_eq!(
            ItemSet::new(),
            grammar.goto(&ItemSet::from(Item::Start), Symbol::Terminal(a))
        );

        // {S -> a . X, X -> . b}
        let example = {
            let mut set = ItemSet::new();
            set.push(Item::Rule(&grammar.rules[0], 1));
            set.push(Item::Rule(&grammar.rules[1], 0));
            set
        };

        // GOTO({S -> a . X, X -> . b}, X) = {S -> a X .}
        assert_eq!(
            ItemSet::from(Item::Rule(&grammar.rules[0], 2)),
            grammar.goto(&example, Symbol::Nonterminal(x))
        );

        // GOTO({S -> a . X, X -> . b}, b) = {X -> b .}
        assert_eq!(
            ItemSet::from(Item::Rule(&grammar.rules[1], 1)),
            grammar.goto(&example, Symbol::Terminal(b))
        );
    }

    #[test]
    fn can_generate_item_sets_for_minimal_grammar() {
        // S -> a
        let (start, mut grammar) = Grammar::new();
        let a = grammar.add_terminal("a");
        grammar.add_rule(start).terminal(a);
        let grammar = grammar.validate().unwrap();

        let (sets, gotos) = grammar.item_sets();
        assert_eq!(3, sets.len());

        // S' -> . S
        // S -> . a
        assert_eq!(2, sets[0].len());
        assert!(sets[0].contains(&Item::Start));
        assert!(sets[0].contains(&Item::Rule(&grammar.rules[0], 0)));

        // S' -> S .
        assert_eq!(1, sets[1].len());
        assert!(sets[1].contains(&Item::End));

        // S -> a .
        assert_eq!(1, sets[2].len());
        assert!(sets[2].contains(&Item::Rule(&grammar.rules[0], 1)));

        // GOTO(0, S) = 1
        // GOTO(0, a) = 2
        assert_eq!(2, gotos.len());
        assert_eq!(1, gotos[&(0, Symbol::Nonterminal(start))]);
        assert_eq!(2, gotos[&(0, Symbol::Terminal(a))]);
    }

    #[test]
    fn can_generate_item_sets_for_simple_grammar() {
        // S' -> Expr
        // Expr -> Term + Expr
        // Expr -> Term
        // Term -> int
        // Term -> ( Expr )
        let (expr, mut grammar) = Grammar::new();
        let term = grammar.add_nonterminal("Term");
        let plus = grammar.add_terminal("+");
        let int = grammar.add_terminal("int");
        let open = grammar.add_terminal("(");
        let close = grammar.add_terminal(")");
        grammar
            .add_rule(expr)
            .nonterminal(term)
            .terminal(plus)
            .nonterminal(expr);
        grammar.add_rule(expr).nonterminal(term);
        grammar.add_rule(term).terminal(int);
        grammar
            .add_rule(term)
            .terminal(open)
            .nonterminal(expr)
            .terminal(close);
        let grammar = grammar.validate().unwrap();

        let (sets, gotos) = grammar.item_sets();
        assert_eq!(9, sets.len());

        assert_eq!(5, sets[0].len());
        assert!(sets[0].contains(&Item::Start)); // S' -> . Expr
        assert!(sets[0].contains(&Item::Rule(&grammar.rules[0], 0))); // Expr -> . Term + Expr
        assert!(sets[0].contains(&Item::Rule(&grammar.rules[1], 0))); // Expr -> . Term
        assert!(sets[0].contains(&Item::Rule(&grammar.rules[2], 0))); // Term -> . int
        assert!(sets[0].contains(&Item::Rule(&grammar.rules[3], 0))); // Term -> . ( Expr )

        assert_eq!(1, sets[1].len());
        assert!(sets[1].contains(&Item::End)); // S' -> Expr .

        assert_eq!(2, sets[2].len());
        assert!(sets[2].contains(&Item::Rule(&grammar.rules[0], 1))); // Expr -> Term . + Expr
        assert!(sets[2].contains(&Item::Rule(&grammar.rules[1], 1))); // Expr -> Term .

        assert_eq!(1, sets[3].len());
        assert!(sets[3].contains(&Item::Rule(&grammar.rules[2], 1))); // Term -> int .

        assert_eq!(5, sets[4].len());
        assert!(sets[4].contains(&Item::Rule(&grammar.rules[3], 1))); // Term -> ( . Expr )
        assert!(sets[4].contains(&Item::Rule(&grammar.rules[0], 0))); // Expr -> . Term + Expr
        assert!(sets[4].contains(&Item::Rule(&grammar.rules[1], 0))); // Expr -> . Term
        assert!(sets[4].contains(&Item::Rule(&grammar.rules[2], 0))); // Term -> . int
        assert!(sets[4].contains(&Item::Rule(&grammar.rules[3], 0))); // Term -> . ( Expr )

        assert_eq!(5, sets[5].len());
        assert!(sets[5].contains(&Item::Rule(&grammar.rules[0], 2))); // Expr -> Term + . Expr
        assert!(sets[5].contains(&Item::Rule(&grammar.rules[0], 0))); // Expr -> . Term + Expr
        assert!(sets[5].contains(&Item::Rule(&grammar.rules[1], 0))); // Expr -> . Term
        assert!(sets[5].contains(&Item::Rule(&grammar.rules[2], 0))); // Term -> . int
        assert!(sets[5].contains(&Item::Rule(&grammar.rules[3], 0))); // Term -> . ( Expr )

        assert_eq!(1, sets[6].len());
        assert!(sets[6].contains(&Item::Rule(&grammar.rules[3], 2))); // Term -> ( Expr . )

        assert_eq!(1, sets[7].len());
        assert!(sets[7].contains(&Item::Rule(&grammar.rules[0], 3))); // Expr -> Term + Expr .

        assert_eq!(1, sets[8].len());
        assert!(sets[8].contains(&Item::Rule(&grammar.rules[3], 3))); // Term -> ( Expr ) .

        assert_eq!(14, gotos.len());
        assert_eq!(1, gotos[&(0, Symbol::Nonterminal(expr))]);
        assert_eq!(2, gotos[&(0, Symbol::Nonterminal(term))]);
        assert_eq!(3, gotos[&(0, Symbol::Terminal(int))]);
        assert_eq!(4, gotos[&(0, Symbol::Terminal(open))]);
        assert_eq!(5, gotos[&(2, Symbol::Terminal(plus))]);
        assert_eq!(6, gotos[&(4, Symbol::Nonterminal(expr))]);
        assert_eq!(2, gotos[&(4, Symbol::Nonterminal(term))]);
        assert_eq!(3, gotos[&(4, Symbol::Terminal(int))]);
        assert_eq!(4, gotos[&(4, Symbol::Terminal(open))]);
        assert_eq!(7, gotos[&(5, Symbol::Nonterminal(expr))]);
        assert_eq!(2, gotos[&(5, Symbol::Nonterminal(term))]);
        assert_eq!(3, gotos[&(5, Symbol::Terminal(int))]);
        assert_eq!(8, gotos[&(6, Symbol::Terminal(close))]);
    }
}
