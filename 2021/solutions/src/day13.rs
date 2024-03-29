//! Transparent Origami

use ahash::AHashSet;
use itertools::Itertools;

fn fold_points(grid: AHashSet<(u32, u32)>, folds: &[(char, u32)]) -> AHashSet<(u32, u32)> {
    let mut next = AHashSet::new();
    for (mut x, mut y) in grid {
        for (axis, k) in folds {
            x = if x < *k || *axis == 'y' { x } else { k * 2 - x };
            y = if y < *k || *axis == 'x' { y } else { k * 2 - y };
        }
        next.insert((x, y));
    }
    next
}

pub fn solve(input: String) -> (usize, String) {
    let mut input = input.lines();
    let mut grid = AHashSet::<(u32, u32)>::new();
    for line in &mut input {
        if line.is_empty() {
            break;
        }
        let (x, y) = line.split_once(',').unwrap();
        grid.insert((x.parse().unwrap(), y.parse().unwrap()));
    }

    let folds: Vec<(char, u32)> = input
        .map(|line| {
            let (axis, k) = line[11..].split_once('=').unwrap();
            (axis.chars().next().unwrap(), k.parse().unwrap())
        })
        .collect();

    let grid = fold_points(grid, &folds[0..1]);
    let part1 = grid.len();

    let grid = fold_points(grid, &folds[1..]);
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
