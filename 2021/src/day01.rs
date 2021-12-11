//! Sonar Sweep

use crate::aoc::Itertools;

fn answer(data: &[i32], window: usize) -> usize {
    data.windows(window)
        .tuple_windows()
        .filter(|(x, y)| y.iter().sum::<i32>() > x.iter().sum())
        .count()
}

pub fn solve(data: Vec<i32>) -> (usize, usize) {
    (answer(&data, 1), answer(&data, 3))
}
