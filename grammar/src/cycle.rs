//! Utility module of a cycle-finding algorithm.
//!
//! A depth-first search is performed over a directed graph. When a vertex which has been seen
//! before is found, it is kept track of as a 'building cycle.' When returning from a child node
//! during the search, these cycles are updated with the parent node. If this parent node completes
//! the cycle, it becomes a 'full cycle' and is not updated anymore. Otherwise, the node is
//! prepended to the cycle.

/// For a directed graph with vertices `0..count`, where `neighbour` defines whether two vertices
/// are connected, return the sets of vertices which are in cycles.
pub fn find_cycles(count: usize, is_neighbour: impl Fn(usize, usize) -> bool) -> Vec<Vec<usize>> {
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
enum Cycle {
    Building(usize, Vec<usize>),
    Full(Vec<usize>),
}

impl Cycle {
    /// Update a cycle with a vertex from the search stack. If the vertex 'completes' the cycle,
    /// transisition to a 'full cycle,' otherwise prepend it to the list of vertices.
    pub fn update(self, vertex: usize) -> Self {
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

struct Search<F: Fn(usize, usize) -> bool> {
    vertices: Vec<VertexState>,
    is_neighbour: F,
}

impl<F: Fn(usize, usize) -> bool> Search<F> {
    pub fn new(count: usize, is_neighbour: F) -> Self {
        Self {
            vertices: vec![VertexState::Unvisited; count],
            is_neighbour,
        }
    }

    pub fn execute(mut self) -> Vec<Vec<usize>> {
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

    fn search(&mut self, v: usize) -> Vec<Cycle> {
        debug_assert!(v < self.vertices.len());

        match self.vertices[v] {
            VertexState::Unvisited => {
                self.vertices[v] = VertexState::Visited;

                let mut cycles = Vec::new();

                for w in 0..self.vertices.len() {
                    if (self.is_neighbour)(v, w) {
                        for cycle in self.search(w) {
                            cycles.push(cycle.update(v));
                        }
                    }
                }

                self.vertices[v] = VertexState::Finished;
                cycles
            }

            VertexState::Visited => vec![Cycle::Building(v, vec![])],

            VertexState::Finished => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::find_cycles;

    #[test]
    fn empty_graph_has_no_cycles() {
        let cycles = find_cycles(0, |_, _| false);
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
