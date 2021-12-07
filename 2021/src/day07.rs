//! The Treachery of Whales

use crate::aoc::*;

fn find_min(iter: impl Iterator<Item = i32>) -> i32 {
    for (x, y) in iter.tuple_windows() {
        if y > x {
            return x;
        }
    }
    panic!();
}

pub fn solve(input: Vec<String>) -> (i32, i32) {
    let crabs = numbers(&input[0], ',').collect::<Vec<_>>();
    let max: i32 = *crabs.iter().max().unwrap();

    let part1 = (0..=max).map(|i| crabs.iter().map(|x| (x - i).abs()).sum::<i32>());
    let part2 = (0..=max).map(|i| {
        crabs
            .iter()
            .map(|x| (x - i).abs())
            .map(|x| x * (x + 1) / 2)
            .sum::<i32>()
    });

    (find_min(part1), find_min(part2))
}
