//! Passage Pathing

// This code encodes "small" caves as negative numbers and "large" caves as
// positive numbers. We use the fact that there are very few nodes to encode the
// visit state into a u32 integer.

use itertools::Itertools;
use std::collections::BTreeMap;

// These are arbitary (but compatible with the string interning mechanism).
const START: i8 = 1;
const END: i8 = 0;

fn visit(graph: &[Vec<i8>], visited: u32, node: i8, allow_twice: bool) -> i64 {
    graph[node.abs() as usize]
        .iter()
        .cloned()
        .map(|adj| {
            let seen = visited & (1 << adj.abs()) > 0;
            if adj < 0 && !(seen && allow_twice) {
                visit(graph, visited | (1 << adj.abs()), adj, seen || allow_twice)
            } else if adj > 0 {
                visit(graph, visited, adj, allow_twice)
            } else if adj == END {
                1
            } else {
                0
            }
        })
        .sum()
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
        visit(&graph, 1 << START, START, true),
        visit(&graph, 1 << START, START, false),
    )
}
