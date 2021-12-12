//! Passage Pathing

// This code encodes "small" caves as negative numbers and "large" caves as
// positive numbers. This is fast and also allows us to process the caves in
// batches by sorting the adjacency list. We also use the fact that there are
// very few nodes to encode the visit state into a u32 integer.

use itertools::Itertools;
use std::collections::BTreeMap;

const START: i8 = 1;
const END: i8 = 0;

fn visit(graph: &[Vec<i8>], visited: u32, node: i8, visited_twice: bool) -> i64 {
    let edges = &graph[node.abs() as usize];
    let mut i = 0;
    let mut sum = 0;

    // Small caves.
    while i < edges.len() && edges[i] < 0 {
        let seen = visited & (1 << edges[i].abs()) > 0;
        if !seen || !visited_twice {
            sum += visit(
                graph,
                visited | (1 << edges[i].abs()),
                edges[i],
                seen || visited_twice,
            );
        }
        i += 1;
    }

    // End.
    if edges.get(i) == Some(&END) {
        sum += 1;
        i += 1;
    }

    // Big caves.
    while i < edges.len() {
        sum += visit(graph, visited, edges[i], visited_twice);
        i += 1;
    }
    sum
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
    for node in 0..strings.len() {
        graph[node].sort_unstable();
    }

    (
        visit(&graph, 1 << START, START, true),
        visit(&graph, 1 << START, START, false),
    )
}
