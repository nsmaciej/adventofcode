//! The Treachery of Whales

use crate::utils::numbers;

pub fn solve(input: Vec<String>) -> (i32, i32) {
    let mut crabs: Vec<i32> = numbers(&input[0], ',').collect();

    let midpoint = crabs.len() / 2;
    let median = *crabs.select_nth_unstable(midpoint).1;

    // As Reddit points out, you need to check both floor and ceil.
    let sum: i32 = crabs.iter().sum();
    let mean_floor = sum / crabs.len() as i32;
    let mean_ceil = (sum + 1) / crabs.len() as i32;

    let with_mean = |mean: i32| -> i32 {
        crabs
            .iter()
            .map(|x| (x - mean).abs())
            .map(|x| x * (x + 1) / 2)
            .sum()
    };

    (
        crabs.iter().map(|x| (x - median).abs()).sum(),
        with_mean(mean_floor).min(with_mean(mean_ceil)),
    )
}
