#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Library for validating grammars and generating parse tables.

mod cycle;

use crate::cycle::find_cycles;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

/// A type which is used to represent a nonterminal during parsing.
pub trait Nonterminal: Debug + Copy + Ord + Hash {
    /// The type returned by `Iterable::all)()`.
    type Iterator: Iterator<Item = Self>;

    /// The number of values in this set.
    const COUNT: usize;

    /// All values in this set.
    fn all() -> Self::Iterator;

    /// Convert a value into its index.
    fn index(self) -> usize;

    /// Get a value by its index.
    fn from_index(index: usize) -> Self;

    /// Get the string representation of a value.
    fn as_str(&self) -> &'static str;
}

/// A type which is used to represent a terminal during parsing.
pub trait Terminal: Debug + Copy + Eq + Ord + Hash {
    /// The type returned by `Iterable::all)()`.
    type Iterator: Iterator<Item = Self>;

    /// The number of values in this set.
    const COUNT: usize;

    /// All values in this set.
    fn all() -> Self::Iterator;

    /// Convert a value into its index.
    fn index(self) -> usize;

    /// Get a value by its index.
    fn from_index(index: usize) -> Self;

    /// Get the string representation of a value.
    fn as_str(&self) -> &'static str;
}

impl<N: Nonterminal> cycle::Node for N {
    fn from_usize(value: usize) -> Self {
        N::from_index(value)
    }
}

/// A context-free grammar.
#[derive(Debug)]
pub struct Grammar<N, T> {
    start: N,
    rules: Vec<(N, Vec<Symbol<N, T>>)>,
}

impl<N: Nonterminal, T: Terminal> Grammar<N, T> {
    /// Create a new grammar, returning its start nonterminal and itself.
    pub fn new(start: N) -> Self {
        Self {
            start,
            rules: Vec::new(),
        }
    }

    /// Add a rule to the grammar.
    pub fn add_rule(&mut self, left: N) -> GrammarRule<N, T> {
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
    pub fn validate(self) -> Result<ProperGrammar<N, T>, Error<N, T>> {
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
            start: self.start,
            firsts,
            follows,
            rules: rules.into_boxed_slice(),
        })
    }

    /// Validate that the grammar contains no unproductive nonterminals. See [`Self::validate`].
    fn validate_productivity(&self) -> Result<(), Error<N, T>> {
        let mut productive = vec![false; N::COUNT];

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                let mut is_definitely_productive = true;

                for r in right {
                    if let Symbol::Nonterminal(n) = r {
                        if !productive[n.index()] {
                            is_definitely_productive = false;
                            break;
                        }
                    }
                }

                if is_definitely_productive && !productive[left.index()] {
                    productive[left.index()] = true;
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
                unproductive_nonterminals.push(N::from_index(p));
            }
        }

        if unproductive_nonterminals.is_empty() {
            Ok(())
        } else {
            Err(Error::UnproductiveNonterminals(unproductive_nonterminals))
        }
    }

    /// Validate that the grammar contains no unreachable symbols. See [`Self::validate`].
    fn validate_reachability(&self) -> Result<(), Error<N, T>> {
        let mut reachable_nonterminal = vec![false; N::COUNT];
        let mut reachable_terminal = vec![false; T::COUNT];

        reachable_nonterminal[0] = true; // start nonterminal is reachable by definition

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                if !reachable_nonterminal[left.index()] {
                    continue;
                }

                for r in right {
                    let reachability = match r {
                        Symbol::Nonterminal(n) => &mut reachable_nonterminal[n.index()],
                        Symbol::Terminal(t) => &mut reachable_terminal[t.index()],
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
                nonterminals.push(N::from_index(n));
            }
        }

        let mut terminals = Vec::new();
        for (t, is_reachable) in reachable_terminal.into_iter().enumerate() {
            if !is_reachable {
                terminals.push(T::from_index(t));
            }
        }

        if nonterminals.is_empty() && terminals.is_empty() {
            Ok(())
        } else {
            Err(Error::UnreachableSymbols(nonterminals, terminals))
        }
    }

    /// Validate that the grammar contains no epsilon productions. See [`Self::validate`].
    fn validate_epsilon_productions(&self) -> Result<(), Error<N, T>> {
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
    fn validate_acyclic(&self) -> Result<(), Error<N, T>> {
        let is_neighbour = |a: N, b: N| {
            self.rules.iter().any(|(c, d)| {
                let left_match = c == &a;
                let right_match = (d.len() == 1) && (d[0] == Symbol::Nonterminal(b));
                left_match && right_match
            })
        };

        let cycles = find_cycles(N::COUNT, is_neighbour);

        if cycles.is_empty() {
            Ok(())
        } else {
            Err(Error::ContainsCycles(cycles))
        }
    }

    fn calculate_firsts(&self) -> Vec<Vec<T>> {
        let mut sets = vec![Vec::new(); N::COUNT];

        loop {
            let mut changed = false;

            for (left, right) in &self.rules {
                // This assumes that no symbols are nullable, and therefore only the first symbol
                // of a rewrite rule has to be checked.
                match right.get(0) {
                    Some(&Symbol::Nonterminal(n)) => {
                        let current_items = &sets[left.index()];
                        let mut to_add = Vec::new();

                        for &s in &sets[n.index()] {
                            if !current_items.contains(&s) {
                                to_add.push(s);
                                changed = true;
                            }
                        }

                        sets[left.index()].extend(to_add);
                    }
                    Some(&Symbol::Terminal(t)) => {
                        let set = &mut sets[left.index()];
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

    fn calculate_follows(&self, firsts: &[Vec<T>]) -> Vec<Vec<Option<T>>> {
        let mut follows = vec![Vec::new(); N::COUNT];

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
                                for &y in &firsts[x.index()] {
                                    if !follows[n.index()].contains(&Some(y)) {
                                        follows[n.index()].push(Some(y));
                                        changed = true;
                                    }
                                }
                            }
                            &Symbol::Terminal(t) => {
                                if !follows[n.index()].contains(&Some(t)) {
                                    follows[n.index()].push(Some(t));
                                    changed = true;
                                }
                            }
                        }
                    }
                }

                if let Some(Symbol::Nonterminal(n)) = right.last() {
                    let current_items = &follows[n.index()];
                    let mut new_items = Vec::new();

                    for &item in &follows[left.index()] {
                        if !current_items.contains(&item) {
                            new_items.push(item);
                            changed = true;
                        }
                    }

                    follows[n.index()].extend(new_items);
                }
            }

            if !changed {
                break;
            }
        }

        follows
    }
}

impl<N: Nonterminal, T: Terminal> std::fmt::Display for Grammar<N, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (left, right) in &self.rules {
            write!(f, "{} ->", left.as_str())?;

            for r in right {
                let string = match r {
                    Symbol::Nonterminal(n) => n.as_str(),
                    Symbol::Terminal(t) => t.as_str(),
                };
                write!(f, " {}", string)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

/// A rewrite rule of a context-free grammar.
pub struct GrammarRule<'a, N, T> {
    rule: &'a mut (N, Vec<Symbol<N, T>>),
}

impl<'a, N, T> GrammarRule<'a, N, T> {
    /// Append a terminal to the rule.
    pub fn terminal(self, t: T) -> Self {
        self.symbol(Symbol::Terminal(t))
    }

    /// Append a nonterminal to the rule.
    pub fn nonterminal(self, n: N) -> Self {
        self.symbol(Symbol::Nonterminal(n))
    }

    fn symbol(self, s: Symbol<N, T>) -> Self {
        self.rule.1.push(s);
        self
    }
}

type Rule<N, T> = (N, Box<[Symbol<N, T>]>);

/// A proper context-free grammar. See [`Grammar::validate`].
#[derive(Debug)]
pub struct ProperGrammar<N, T> {
    start: N,
    // TODO: Consider using a hash set instead?
    firsts: Vec<Vec<T>>,
    // TODO: Again, consider the HashSet.
    follows: Vec<Vec<Option<T>>>,
    rules: Box<[Rule<N, T>]>,
}

type GotoMap<N, T> = HashMap<(usize, Symbol<N, T>), usize>;

impl<N: Nonterminal, T: Terminal> ProperGrammar<N, T> {
    /// Return the set of terminals which may begin strings derived from `symbols.`
    ///
    /// TODO: Consider returning a slice of precomputed values.
    pub fn first(&self, symbols: &[Symbol<N, T>]) -> Vec<T> {
        match symbols.get(0) {
            Some(&Symbol::Nonterminal(n)) => self.firsts[n.index()].clone(),
            Some(&Symbol::Terminal(t)) => vec![t],
            None => Vec::new(),
        }
    }

    /// Return the FOLLOW set of terminals which may follow `symbol` in a derivation. `None` is
    /// used to encode the endmarker.
    pub fn follow(&self, nonterminal: N) -> Vec<Option<T>> {
        self.follows[nonterminal.index()].clone()
    }

    /// Generate the item sets for this grammar. The GOTO state function is also provided.
    pub fn item_sets(&self) -> (Vec<ItemSet<N, T>>, GotoMap<N, T>) {
        let set_0 = self.closure(ItemSet::from(Item::Start));
        let mut collection = vec![set_0];
        let mut gotos = HashMap::new();

        let mut changed = true;
        while changed {
            changed = false;

            // TODO: Not efficient, optimize this at some point.
            let sets = collection.clone().into_iter().enumerate();
            for (i, set) in sets {
                for symbol in Self::symbols() {
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

    fn symbols() -> impl Iterator<Item = Symbol<N, T>> {
        let nonterminals = N::all().map(|n| Symbol::Nonterminal(n));
        let terminals = T::all().map(|t| Symbol::Terminal(t));

        nonterminals.chain(terminals)
    }

    /// Return the symbol after the dot for the given item.
    fn next_symbol(&self, item: Item<N, T>) -> Option<Symbol<N, T>> {
        match item {
            Item::Start => Some(Symbol::Nonterminal(self.start)),

            Item::End => None,

            Item::Rule((_, body), dot) => body.get(dot).copied(),
        }
    }

    fn closure<'a>(&'a self, mut item_set: ItemSet<'a, N, T>) -> ItemSet<N, T> {
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

    fn goto<'a>(&'a self, item_set: &ItemSet<'a, N, T>, symbol: Symbol<N, T>) -> ItemSet<N, T> {
        let mut goto_set = ItemSet::new();

        for &item in &item_set.items {
            if let (Some(next), Some(s)) = (item.next(), self.next_symbol(item)) {
                if symbol == s {
                    goto_set.push(next);
                }
            }
        }

        self.closure(goto_set)
    }

    /// Generate an SLR parse table for the grammar. (TODO: LALR parse table generation.)
    pub fn parse_table(&self) -> Result<ParseTable<N, T>, ParseTableConflict> {
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
        symbols: HashMap<(usize, Symbol<N, T>), usize>,
    ) -> HashMap<(usize, N), usize> {
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
pub enum ParseAction<N> {
    /// Shift a token from the input, pushing this state onto the stack.
    Shift(usize),

    /// Reduce this many symbols into the nonterminal.
    Reduce(N, usize),

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
pub enum Error<N, T> {
    /// One or more nonterminals can never produce a string of terminals during derivation.
    UnproductiveNonterminals(Vec<N>),

    /// One or more symbols are can not be produced by a derivation from the start symbol.
    UnreachableSymbols(Vec<N>, Vec<T>),

    /// One or more nonterminals can derive empty strings.
    EpsilonProductions(Vec<N>),

    /// One or more nonterminals can derive themselves exactly.
    ContainsCycles(Vec<Vec<N>>),
}

/// A symbol of a grammar, either terminal or nonterminal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol<N, T> {
    /// A nonterminal symbol.
    Nonterminal(N),

    /// A terminal symbol.
    Terminal(T),
}

impl<N: Nonterminal, T: Terminal> Symbol<N, T> {
    /// Get the string representation of the underlying value.
    pub fn as_str(&self) -> &'static str {
        match self {
            Symbol::Nonterminal(n) => n.as_str(),
            Symbol::Terminal(t) => t.as_str(),
        }
    }
}

/// A grammar item, representing a partially parsed production rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Item<'a, N, T> {
    /// The item `S' -> . S`.
    Start,

    /// The item `S' -> S .`
    End,

    /// A non-start item.
    /// TODO: Move to Rule typedef.
    Rule(&'a (N, Box<[Symbol<N, T>]>), usize),
}

impl<'a, N, T> Item<'a, N, T> {
    fn next(self) -> Option<Self> {
        match self {
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
}

/// A set of grammar items.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct ItemSet<'a, N, T> {
    // TODO: Consider not storing non-kernel items?
    // TODO: Consider using a HashSet?
    items: Vec<Item<'a, N, T>>,
}

impl<'a, N: Nonterminal, T: Terminal> ItemSet<'a, N, T> {
    /// Create a new, empty set.
    #[must_use]
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Whether the set contains zero elements.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.items.len() == 0
    }

    /// The number of items the set contains.
    #[must_use]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Push an item into the set, returning whether it was newly inserted.
    pub fn push(&mut self, item: Item<'a, N, T>) -> bool {
        if let Err(index) = self.items.binary_search(&item) {
            self.items.insert(index, item);
            true
        } else {
            false
        }
    }

    /// Whether the set contains the given item.
    #[must_use]
    pub fn contains(&self, item: &Item<N, T>) -> bool {
        self.items.binary_search(item).is_ok()
    }
}

impl<'a, N, T> IntoIterator for ItemSet<'a, N, T> {
    type Item = Item<'a, N, T>;

    type IntoIter = <Vec<Item<'a, N, T>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a, N, T> From<Item<'a, N, T>> for ItemSet<'a, N, T> {
    fn from(item: Item<'a, N, T>) -> Self {
        Self { items: vec![item] }
    }
}

/// A parse table used for shift/reduce decisions during parsing.
#[derive(Debug)]
pub struct ParseTable<N, T> {
    // TODO: Consider 2D arrays? Benchmark
    actions: HashMap<(usize, Option<T>), ParseAction<N>>,
    gotos: HashMap<(usize, N), usize>,
}

type ParseErrorInfo<T, I> = (ParseError<T>, Option<I>);

impl<N: Nonterminal, T: Terminal> ParseTable<N, T> {
    /// The corresponding action for the given state and input. `None` represents the endmarker.
    pub fn action(&self, state: usize, input: Option<T>) -> ParseAction<N> {
        self.actions
            .get(&(state, input))
            .copied()
            .unwrap_or(ParseAction::Error)
    }

    /// The state to transition to given an input and state.
    pub fn goto(&self, state: usize, input: N) -> usize {
        assert!(
            self.gotos.contains_key(&(state, input)),
            "Bad goto for state {} and input {}",
            state,
            input.as_str()
        );

        self.gotos[&(state, input)]
    }

    /// Parse a sequence of terminals and additional information into a parse tree.
    pub fn parse<I: Copy, M>(
        &self,
        mut input: impl Iterator<Item = (T, I)>,
        reduce: impl Fn(N, Vec<ParseTree<M, T, I>>) -> M,
    ) -> Result<M, ParseErrorInfo<T, I>> {
        // Dummy parse tree for the initial state, never gets touched, just needs to exist.
        let mut a = input.next();
        let mut stack = Vec::new();

        loop {
            let head_state = match stack.last() {
                Some(&(_, state)) => state,
                None => 0,
            };

            match self.action(head_state, a.map(|x| x.0)) {
                ParseAction::Shift(t) => {
                    let (token, info) = a.unwrap();
                    stack.push((ParseTree::Terminal(token, info), t));
                    a = input.next();
                }

                ParseAction::Reduce(a, len) => {
                    let mut subtrees = Vec::with_capacity(len);
                    let remove_index = stack.len() - len;
                    for _ in 0..len {
                        let (item, _) = stack.remove(remove_index);
                        subtrees.push(item);
                    }

                    let tree = ParseTree::Nonterminal(reduce(a, subtrees));

                    let t = match stack.last() {
                        Some(&(_, state)) => state,
                        None => 0,
                    };

                    stack.push((tree, self.goto(t, a)));
                }

                ParseAction::Error => {
                    let mut expected = HashSet::new();
                    for &(state, input) in self.actions.keys() {
                        if state == head_state {
                            expected.insert(input);
                        }
                    }

                    let (actual, info) = match a.take() {
                        Some((t, i)) => (Some(t), Some(i)),
                        None => (None, None),
                    };
                    let error = ParseError { expected, actual };
                    return Err((error, info));
                }

                ParseAction::Accept => break,
            }
        }

        assert_eq!(1, stack.len());

        match stack.pop() {
            Some((ParseTree::Nonterminal(root), _)) => Ok(root),
            _ => panic!(),
        }
    }
}

/// A tree of symbols generated during parsing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseTree<N, T, I> {
    /// A terminal node and position information, corresponding to an input symbol.
    Terminal(T, I),

    /// A nonterminal node, containing subtrees.
    Nonterminal(N),
}

/// An error encountered during parsing.
#[derive(Clone, Debug)]
pub struct ParseError<T: Terminal> {
    expected: HashSet<Option<T>>,
    actual: Option<T>,
}

impl<T: Terminal> ParseError<T> {
    /// The set of expected inputs (the endmarker represented by `None`).
    pub fn expected(&self) -> &HashSet<Option<T>> {
        &self.expected
    }

    /// The erroneous input (the endmarker represented by `None`).
    pub fn actual(&self) -> Option<T> {
        self.actual
    }
}

impl<T: Terminal> std::fmt::Display for ParseError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let actual_str = match self.actual {
            Some(terminal) => terminal.as_str(),
            None => "EOF",
        };

        if self.expected.is_empty() {
            panic!("error from 0-expectation state, this is a bug");
        } else if self.expected.len() == 1 {
            let expected = self.expected.iter().next().unwrap();
            let expected_str = match expected {
                Some(terminal) => terminal.as_str(),
                None => "EOF",
            };
            write!(f, "expected {} but found {}", expected_str, actual_str)
        } else {
            let expected: Vec<_> = self
                .expected
                .iter()
                .map(|item| match item {
                    Some(terminal) => terminal.as_str(),
                    None => "EOF",
                })
                .collect();

            let expected_str = expected.join(", ");

            write!(
                f,
                "expected one of: {} but found {}",
                expected_str, actual_str
            )
        }
    }
}

impl<T: Terminal> std::error::Error for ParseError<T> {}

#[cfg(test)]
mod tests {
    use super::{Grammar, Item, ItemSet, Nonterminal, Symbol, Terminal};

    /// Example grammar.
    /// `S -> X Y`
    /// `S -> X`
    /// `X -> c X b`
    /// `X -> d`
    /// `Y -> Y a`
    /// `Y -> b`
    mod example {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum N {
            S,
            X,
            Y,
        }

        impl crate::Nonterminal for N {
            type Iterator = std::array::IntoIter<Self, 3>;

            const COUNT: usize = 3;

            fn all() -> Self::Iterator {
                [Self::S, Self::X, Self::Y].into_iter()
            }

            fn index(self) -> usize {
                self as usize
            }

            fn from_index(index: usize) -> Self {
                match index {
                    0 => Self::S,
                    1 => Self::X,
                    2 => Self::Y,
                    _ => panic!(),
                }
            }

            fn as_str(&self) -> &'static str {
                match self {
                    N::S => "S",
                    N::X => "X",
                    N::Y => "Y",
                }
            }
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum T {
            A,
            B,
            C,
            D,
        }

        impl crate::Terminal for T {
            type Iterator = std::array::IntoIter<Self, 4>;

            const COUNT: usize = 4;

            fn all() -> Self::Iterator {
                [Self::A, Self::B, Self::C, Self::D].into_iter()
            }

            fn index(self) -> usize {
                self as usize
            }

            fn from_index(index: usize) -> Self {
                match index {
                    0 => Self::A,
                    1 => Self::B,
                    2 => Self::C,
                    3 => Self::D,
                    _ => panic!(),
                }
            }

            fn as_str(&self) -> &'static str {
                match self {
                    T::A => "A",
                    T::B => "B",
                    T::C => "C",
                    T::D => "D",
                }
            }
        }
    }

    #[test]
    fn proper_grammar_takes_grammar_rules() {
        use example::{N, T};
        let mut grammar = Grammar::new(N::S);
        grammar.add_rule(N::S).nonterminal(N::X).nonterminal(N::Y);
        grammar.add_rule(N::S).nonterminal(N::X);
        grammar
            .add_rule(N::X)
            .terminal(T::C)
            .nonterminal(N::X)
            .terminal(T::B);
        grammar.add_rule(N::X).terminal(T::D);
        grammar.add_rule(N::Y).nonterminal(N::Y).terminal(T::A);
        grammar.add_rule(N::Y).terminal(T::B);

        let grammar = grammar.validate().unwrap();

        use Symbol::{Nonterminal as Sn, Terminal as St};

        assert_eq!(
            [
                (N::S, vec![Sn(N::X), Sn(N::Y)].into_boxed_slice()),
                (N::S, vec![Sn(N::X)].into_boxed_slice()),
                (N::X, vec![St(T::C), Sn(N::X), St(T::B)].into_boxed_slice()),
                (N::X, vec![St(T::D)].into_boxed_slice()),
                (N::Y, vec![Sn(N::Y), St(T::A)].into_boxed_slice()),
                (N::Y, vec![St(T::B)].into_boxed_slice()),
            ],
            grammar.rules.as_ref()
        );
    }

    #[test]
    fn can_get_subsequent_nonterminal_from_item() {
        use example::{N, T};
        let mut grammar = Grammar::new(N::S);
        grammar.add_rule(N::S).nonterminal(N::X).nonterminal(N::Y);
        grammar.add_rule(N::S).nonterminal(N::X);
        grammar
            .add_rule(N::X)
            .terminal(T::C)
            .nonterminal(N::X)
            .terminal(T::B);
        grammar.add_rule(N::X).terminal(T::D);
        grammar.add_rule(N::Y).nonterminal(N::Y).terminal(T::A);
        grammar.add_rule(N::Y).terminal(T::B);
        let grammar = grammar.validate().unwrap();

        // S' -> . S ==> S
        let expected = grammar.next_symbol(Item::Start);
        assert_eq!(Some(Symbol::Nonterminal(N::S)), expected);

        // S' -> S' S . ==> (nothing)
        let expected = grammar.next_symbol(Item::End);
        assert_eq!(None, expected);

        // S -> X . Y ==> Y
        let expected = grammar.next_symbol(Item::Rule(&grammar.rules[0], 1));
        assert_eq!(Some(Symbol::Nonterminal(N::Y)), expected);

        // X -> . d ==> d
        let expected = grammar.next_symbol(Item::Rule(&grammar.rules[3], 0));
        assert_eq!(Some(Symbol::Terminal(T::D)), expected);

        // Y -> b . ==> (nothing)
        let expected = grammar.next_symbol(Item::Rule(&grammar.rules[5], 1));
        assert_eq!(None, expected);
    }

    #[test]
    fn can_push_to_item_set() {
        use example::{N, T};

        let mut set = ItemSet::new();
        assert_eq!(0, set.len());

        assert!(set.push(Item::Start));
        assert_eq!(1, set.len());

        assert!(!set.push(Item::Start));
        assert_eq!(1, set.len());

        assert!(set.push(Item::End));
        assert_eq!(2, set.len());

        let rule = (N::S, vec![Symbol::Terminal(T::A)].into_boxed_slice());

        assert!(set.push(Item::Rule(&rule, 0)));
        assert!(set.push(Item::Rule(&rule, 1)));
        assert_eq!(4, set.len());
    }

    #[test]
    fn can_take_closure_of_items() {
        use example::{N, T};

        let mut grammar = crate::Grammar::new(N::S);
        grammar.add_rule(N::S).nonterminal(N::X).nonterminal(N::Y);
        grammar.add_rule(N::S).nonterminal(N::X);
        grammar
            .add_rule(N::X)
            .terminal(T::C)
            .nonterminal(N::X)
            .terminal(T::B);
        grammar.add_rule(N::X).terminal(T::D);
        grammar.add_rule(N::Y).nonterminal(N::Y).terminal(T::A);
        grammar.add_rule(N::Y).terminal(T::B);
        let grammar = grammar.validate().unwrap();

        // S' -> . S
        // S -> . X Y
        // S -> . X
        // X -> . c X b
        // X -> . d
        let set = ItemSet::from(Item::Start);
        let expected = {
            let mut set = ItemSet::from(Item::Start);
            set.push(Item::Rule(&grammar.rules[0], 0));
            set.push(Item::Rule(&grammar.rules[1], 0));
            set.push(Item::Rule(&grammar.rules[2], 0));
            set.push(Item::Rule(&grammar.rules[3], 0));
            set
        };
        assert_eq!(expected, grammar.closure(set));

        // S -> X . Y
        // Y -> . Y a
        // Y -> . b
        let set = ItemSet::from(Item::Rule(&grammar.rules[0], 1));
        let expected = {
            let mut set = ItemSet::from(Item::Rule(&grammar.rules[0], 1));
            set.push(Item::Rule(&grammar.rules[4], 0));
            set.push(Item::Rule(&grammar.rules[5], 0));
            set
        };
        assert_eq!(expected, grammar.closure(set));

        // S' -> S .
        let set = ItemSet::from(Item::End);
        assert_eq!(set.clone(), grammar.closure(set));
    }

    #[test]
    fn can_get_next_item() {
        use example::{N, T};

        // S' -> S
        // X -> a
        let rule = (N::S, vec![Symbol::Terminal(T::A)].into_boxed_slice());

        // S' -> . S ==> S' -> S .
        assert_eq!(Some(Item::<N, T>::End), Item::Start.next());

        // S' -> S . ==> (nothing)
        assert_eq!(None, Item::<N, T>::End.next());

        // S -> . a ==> S -> a .
        assert_eq!(Some(Item::Rule(&rule, 1)), Item::Rule(&rule, 0).next(),);

        // S -> a . ==> (nothing)
        assert_eq!(None, Item::Rule(&rule, 1).next());
    }

    #[test]
    fn can_calculate_item_set_gotos() {
        use example::{N, T};

        // S' -> S
        // S -> a X
        // X -> b
        // S -> Y
        // Y -> c d
        let mut grammar = crate::Grammar::new(N::S);
        grammar.add_rule(N::S).terminal(T::A).nonterminal(N::X);
        grammar.add_rule(N::X).terminal(T::B);
        grammar.add_rule(N::S).nonterminal(N::Y);
        grammar.add_rule(N::Y).terminal(T::C).terminal(T::D);
        let grammar = grammar.validate().unwrap();

        // GOTO({S' -> . S}, S) = {S' -> S .}
        assert_eq!(
            ItemSet::from(Item::End),
            grammar.goto(&ItemSet::from(Item::Start), Symbol::Nonterminal(N::S))
        );

        // GOTO({S' -> S .}, a) = { }
        assert_eq!(
            ItemSet::new(),
            grammar.goto(&ItemSet::from(Item::Start), Symbol::Terminal(T::A))
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
            grammar.goto(&example, Symbol::Nonterminal(N::X))
        );

        // GOTO({S -> a . X, X -> . b}, b) = {X -> b .}
        assert_eq!(
            ItemSet::from(Item::Rule(&grammar.rules[1], 1)),
            grammar.goto(&example, Symbol::Terminal(T::B))
        );
    }

    #[test]
    fn can_generate_item_sets_for_simple_grammar() {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum N {
            Expr,
            Term,
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum T {
            Plus,
            Int,
            Open,
            Close,
        }

        impl Nonterminal for N {
            type Iterator = std::array::IntoIter<Self, 2>;

            const COUNT: usize = 2;

            fn all() -> Self::Iterator {
                [Self::Expr, Self::Term].into_iter()
            }

            fn index(self) -> usize {
                self as usize
            }

            fn from_index(index: usize) -> Self {
                match index {
                    0 => Self::Expr,
                    1 => Self::Term,
                    _ => panic!(),
                }
            }

            fn as_str(&self) -> &'static str {
                match self {
                    N::Expr => "expr",
                    N::Term => "term",
                }
            }
        }

        impl Terminal for T {
            type Iterator = std::array::IntoIter<Self, 4>;

            const COUNT: usize = 4;

            fn all() -> Self::Iterator {
                [Self::Plus, Self::Int, Self::Open, Self::Close].into_iter()
            }

            fn index(self) -> usize {
                self as usize
            }

            fn from_index(index: usize) -> Self {
                match index {
                    0 => Self::Plus,
                    1 => Self::Int,
                    2 => Self::Open,
                    3 => Self::Close,
                    _ => panic!(),
                }
            }

            fn as_str(&self) -> &'static str {
                match self {
                    T::Plus => "'+'",
                    T::Int => "int",
                    T::Open => "'('",
                    T::Close => "')'",
                }
            }
        }

        // S' -> Expr
        // Expr -> Term + Expr
        // Expr -> Term
        // Term -> int
        // Term -> ( Expr )
        let mut grammar = Grammar::new(N::Expr);
        grammar
            .add_rule(N::Expr)
            .nonterminal(N::Term)
            .terminal(T::Plus)
            .nonterminal(N::Expr);
        grammar.add_rule(N::Expr).nonterminal(N::Term);
        grammar.add_rule(N::Term).terminal(T::Int);
        grammar
            .add_rule(N::Term)
            .terminal(T::Open)
            .nonterminal(N::Expr)
            .terminal(T::Close);
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
        assert_eq!(1, gotos[&(0, Symbol::Nonterminal(N::Expr))]);
        assert_eq!(2, gotos[&(0, Symbol::Nonterminal(N::Term))]);
        assert_eq!(3, gotos[&(0, Symbol::Terminal(T::Int))]);
        assert_eq!(4, gotos[&(0, Symbol::Terminal(T::Open))]);
        assert_eq!(5, gotos[&(2, Symbol::Terminal(T::Plus))]);
        assert_eq!(6, gotos[&(4, Symbol::Nonterminal(N::Expr))]);
        assert_eq!(2, gotos[&(4, Symbol::Nonterminal(N::Term))]);
        assert_eq!(3, gotos[&(4, Symbol::Terminal(T::Int))]);
        assert_eq!(4, gotos[&(4, Symbol::Terminal(T::Open))]);
        assert_eq!(7, gotos[&(5, Symbol::Nonterminal(N::Expr))]);
        assert_eq!(2, gotos[&(5, Symbol::Nonterminal(N::Term))]);
        assert_eq!(3, gotos[&(5, Symbol::Terminal(T::Int))]);
        assert_eq!(8, gotos[&(6, Symbol::Terminal(T::Close))]);
    }
}
