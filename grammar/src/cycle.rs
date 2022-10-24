//! Utility module of a cycle-finding algorithm.
//!
//! A depth-first search is performed over a directed graph. When a vertex which has been seen
//! before is found, it is kept track of as a 'building cycle.' When returning from a child node
//! during the search, these cycles are updated with the parent node. If this parent node completes
//! the cycle, it becomes a 'full cycle' and is not updated anymore. Otherwise, the node is
//! prepended to the cycle.

use std::marker::PhantomData;

pub trait Node: Clone + Copy + PartialEq + Eq {
    fn from_usize(_: usize) -> Self;
}

/// For a directed graph with vertices `0..count`, where `neighbour` defines whether two vertices
/// are connected, return the sets of vertices which are in cycles.
pub fn find_cycles<N: Node>(count: usize, is_neighbour: impl Fn(N, N) -> bool) -> Vec<Vec<N>> {
    Search::new(count, is_neighbour).execute()
}

#[derive(Clone, Copy)]
enum VertexState {
    Unvisited,
    Visited,
    Finished,
}

/// A cycle being built by the depth-first search. When being built, it includes the 'provoking
/// vertex,' the vertex which completed the cycle.
enum Cycle<N: Node> {
    Building(N, Vec<N>),
    Full(Vec<N>),
}

impl<N: Node> Cycle<N> {
    /// Update a cycle with a vertex from the search stack. If the vertex 'completes' the cycle,
    /// transisition to a 'full cycle,' otherwise prepend it to the list of vertices.
    pub fn update(self, vertex: N) -> Self {
        match self {
            Cycle::Building(provoking, mut cycle) => {
                cycle.insert(0, vertex);

                if provoking == vertex {
                    Cycle::Full(cycle)
                } else {
                    Cycle::Building(provoking, cycle)
                }
            }

            Cycle::Full(cycle) => Cycle::Full(cycle),
        }
    }
}

struct Search<N: Node, F: Fn(N, N) -> bool> {
    vertices: Vec<VertexState>,
    is_neighbour: F,
    _phantom: PhantomData<N>,
}

impl<N: Node, F: Fn(N, N) -> bool> Search<N, F> {
    pub fn new(count: usize, is_neighbour: F) -> Self {
        Self {
            vertices: vec![VertexState::Unvisited; count],
            is_neighbour,
            _phantom: PhantomData,
        }
    }

    pub fn execute(mut self) -> Vec<Vec<N>> {
        let mut cycles = Vec::new();

        for v in 0..self.vertices.len() {
            for cycle in self.search(v) {
                if let Cycle::Full(c) = cycle {
                    cycles.push(c);
                } else {
                    panic!("Detected cycle is incomplete.");
                }
            }
        }

        cycles
    }

    fn search(&mut self, v: usize) -> Vec<Cycle<N>> {
        debug_assert!(v < self.vertices.len());

        match self.vertices[v] {
            VertexState::Unvisited => {
                self.vertices[v] = VertexState::Visited;

                let mut cycles = Vec::new();

                for w in 0..self.vertices.len() {
                    if (self.is_neighbour)(N::from_usize(v), N::from_usize(w)) {
                        for cycle in self.search(w) {
                            cycles.push(cycle.update(N::from_usize(v)));
                        }
                    }
                }

                self.vertices[v] = VertexState::Finished;
                cycles
            }

            VertexState::Visited => vec![Cycle::Building(N::from_usize(v), vec![])],

            VertexState::Finished => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{find_cycles, Node};

    impl Node for usize {
        fn from_usize(n: usize) -> Self {
            n
        }
    }

    #[test]
    fn empty_graph_has_no_cycles() {
        let cycles = find_cycles(0, |_: usize, _: usize| false);
        assert!(cycles.is_empty());
    }

    #[test]
    fn cyclic_graph_has_cycle() {
        let cycles = find_cycles(3, |a, b| match (a, b) {
            (0, 1) | (1, 2) | (2, 0) => true,
            _ => false,
        });
        assert_eq!([vec![0, 1, 2]], cycles.as_slice());
    }

    #[test]
    fn acyclic_has_no_cycles() {
        let cycles = find_cycles(6, |a, b| match (a, b) {
            (0, 1) | (0, 2) | (1, 3) | (1, 4) | (2, 3) | (2, 5) => true,
            _ => false,
        });
        assert!(cycles.is_empty());
    }

    #[test]
    fn cyclic_has_cycle() {
        let cycles = find_cycles(6, |a, b| match (a, b) {
            (0, 1) | (0, 2) | (1, 3) | (1, 4) | (2, 3) | (2, 5) | (4, 0) => true,
            _ => false,
        });
        assert_eq!([vec![0, 1, 4]], cycles.as_slice());
    }

    #[test]
    fn can_detect_multiple_cycles() {
        let cycles = find_cycles(6, |a, b| match (a, b) {
            (0, 1) | (1, 2) | (2, 0) | (3, 4) | (4, 5) | (5, 3) => true,
            _ => false,
        });
        assert_eq!([vec![0, 1, 2], vec![3, 4, 5]], cycles.as_slice());
    }

    #[test]
    fn can_detect_complex_cycles() {
        let cycles = find_cycles(5, |a, b| match (a, b) {
            (0, 2) | (1, 2) | (2, 3) | (2, 4) | (3, 0) | (4, 1) => true,
            _ => false,
        });
        assert_eq!([vec![0, 2, 3], vec![2, 4, 1]], cycles.as_slice());
    }

    #[test]
    fn can_detect_adjoining_cycles() {
        let cycles = find_cycles(5, |a, b| match (a, b) {
            (0, 4) | (1, 0) | (1, 3) | (2, 1) | (3, 4) | (3, 2) | (4, 1) => true,
            _ => false,
        });
        assert_eq!(
            [vec![0, 4, 1], vec![1, 3, 2], vec![4, 1, 3]],
            cycles.as_slice()
        );
    }
}
