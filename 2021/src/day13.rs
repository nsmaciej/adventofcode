use std::collections::HashSet;

use crate::utils::*;
use itertools::Itertools;

fn fold_grid(grid: HashSet<(u32, u32)>, fold: (char, u32)) -> HashSet<(u32, u32)> {
    let (axis, k) = fold;
    let max_x = (1 + grid.iter().map(|x| x.1).max().unwrap()) / 2 * 2;
    let max_y = (1 + grid.iter().map(|x| x.0).max().unwrap()) / 2 * 2;
    let mut next = HashSet::new();
    match axis {
        'x' => {
            for (y, x) in grid {
                if x <= k {
                    next.insert((y, x));
                } else {
                    next.insert((y, max_x - x));
                }
            }
        }
        'y' => {
            for (y, x) in grid {
                if y <= k {
                    next.insert((y, x));
                } else {
                    next.insert((max_y - y, x));
                }
            }
        }
        _ => panic!("invalid axis"),
    }
    next
}

fn show(grid: &HashSet<(u32, u32)>) {
    let max_x = grid.iter().map(|x| x.1).max().unwrap();
    let max_y = grid.iter().map(|x| x.0).max().unwrap();
    for y in 0..=max_y {
        for x in 0..=max_x {
            print!("{}", if grid.contains(&(y, x)) { "#" } else { " " });
        }
        println!();
    }
}

pub fn solve(input: String) -> (usize, usize) {
    let mut input = input.lines();
    let mut grid = HashSet::<(u32, u32)>::new();
    for line in &mut input {
        if line == "" {
            break;
        }
        let (x, y) = numbers(line, ',').collect_tuple().unwrap();
        grid.insert((y, x));
    }

    let mut folds = Vec::new();
    for line in input {
        let (axis, k) = line[11..].split('=').collect_tuple().unwrap();
        let k: u32 = k.parse().unwrap();
        folds.push((axis.chars().next().unwrap(), k));
    }

    let mut grid = fold_grid(grid, folds[0]);
    let part1 = grid.len();
    for fold in &folds[1..] {
        grid = fold_grid(grid, fold.clone());
    }
    println!();
    show(&grid);
    (part1, 0)
}
