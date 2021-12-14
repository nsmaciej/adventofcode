use std::collections::HashSet;

use crate::utils::*;
use itertools::Itertools;

fn fold_grid(grid: HashSet<(u32, u32)>, fold: (char, u32)) -> HashSet<(u32, u32)> {
    let (axis, k) = fold;
    let mut next = HashSet::new();
    for (x, y) in grid {
        next.insert((
            if x < k || axis == 'y' { x } else { k * 2 - x },
            if y < k || axis == 'x' { y } else { k * 2 - y },
        ));
    }
    next
}

pub fn solve(input: String) -> (usize, String) {
    let mut input = input.lines();
    let mut grid = HashSet::<(u32, u32)>::new();
    for line in &mut input {
        if line == "" {
            break;
        }
        grid.insert(numbers(line, ',').collect_tuple().unwrap());
    }

    let folds = input
        .map(|line| {
            let (axis, k) = line[11..].split('=').collect_tuple().unwrap();
            (axis.chars().next().unwrap(), k.parse().unwrap())
        })
        .collect_vec();

    let grid = fold_grid(grid, folds[0]);
    let part1 = grid.len();
    let grid = folds[1..].iter().fold(grid, |g, x| fold_grid(g, x.clone()));

    let max_y = grid.iter().map(|x| x.1).max().unwrap();
    let max_x = grid.iter().map(|x| x.0).max().unwrap();
    let part2 = (0..=max_y)
        .map(|y| {
            (0..=max_x)
                .map(|x| if grid.contains(&(x, y)) { '#' } else { ' ' })
                .collect::<String>()
        })
        .join("\n");

    (part1, part2)
}
