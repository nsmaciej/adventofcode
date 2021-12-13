//! Passage Pathing

// This code encodes "small" caves as negative numbers and "large" caves as
// positive numbers. This is fast and also allows us to process the caves in
// batches by sorting the adjacency list. We also use the fact that there are
// very few nodes to encode the visit state into a u32 integer.

use itertools::Itertools;
use std::collections::{BTreeMap, HashMap};

const START: i8 = 1;
const END: i8 = 0;

fn visit(
    graph: &[Vec<i8>],
    cache: &mut HashMap<(u32, i8, bool), i64>,
    visited: u32,
    node: i8,
    visited_twice: bool,
) -> i64 {
    if let Some(last) = cache.get(&(visited, node, visited_twice)) {
        return *last;
    }

    let paths = graph[node.abs() as usize]
        .iter()
        .cloned()
        .map(|adj| {
            let seen = visited & (1 << adj.abs()) > 0;
            if adj < 0 && !(seen && visited_twice) {
                visit(
                    graph,
                    cache,
                    visited | (1 << adj.abs()),
                    adj,
                    seen || visited_twice,
                )
            } else if adj > 0 {
                visit(graph, cache, visited, adj, visited_twice)
            } else if adj == END {
                1
            } else {
                0
            }
        })
        .sum();

    cache.insert((visited, node, visited_twice), paths);
    paths
}

pub fn solve(input: Vec<String>) -> (i64, i64) {
    let mut strings = BTreeMap::<String, i8>::new();
    strings.insert("end".to_string(), END);
    strings.insert("start".to_string(), START);

    let mut graph = vec![Vec::new(); input.len()]; // More than enough.
    for line in input {
        let intern = |x: &str| {
            let small = x.chars().next().unwrap().is_lowercase();
            let id = strings.len() as i8;
            *strings
                .entry(x.to_string())
                .or_insert(if small { -id } else { id })
        };
        let (a, b) = line.split('-').map(intern).collect_tuple().unwrap();
        // Do not link back to start.
        if b != START {
            graph[a.abs() as usize].push(b);
        }
        if a != START {
            graph[b.abs() as usize].push(a);
        }
    }

    (
        visit(&graph, &mut HashMap::new(), 1 << START, START, true),
        visit(&graph, &mut HashMap::new(), 1 << START, START, false),
    )
}
