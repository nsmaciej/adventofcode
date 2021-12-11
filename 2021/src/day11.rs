//! Dumbo Octopus

use crate::utils::Grid;

fn around(y: usize, x: usize) -> [(i64, i64); 8] {
    let y = y as i64;
    let x = x as i64;
    [
        (y - 1, x - 1),
        (y - 1, x),
        (y - 1, x + 1),
        (y, x + 1),
        (y + 1, x + 1),
        (y + 1, x),
        (y + 1, x - 1),
        (y, x - 1),
    ]
}

fn flood(grid: &mut [[i8; 10]; 10], ys: usize, xs: usize) {
    // The compiler can't actually see through `around` here, but in practice it
    // doesn't matter much.
    for (y, x) in around(ys, xs) {
        let Some(cell) = grid.getyx_mut(y as usize, x as usize) else {
            continue; // Out of bounds.
        };
        if *cell == -1 {
            continue; // Already flashed.
        }
        *cell += 1;
        if *cell > 9 {
            *cell = -1; // Flash!
            flood(grid, y as usize, x as usize);
        }
    }
}

fn step(grid: &mut [[i8; 10]; 10]) -> usize {
    grid.iter_mut().flatten().for_each(|x| *x += 1);

    for y in 0..grid.width() {
        for x in 0..grid.height() {
            if grid[y][x] > 9 && grid[y][x] != -1 {
                grid[y][x] = -1; // Flash!
                flood(grid, y, x);
            }
        }
    }

    grid.iter_mut()
        .flatten()
        .filter(|x| **x == -1)
        .map(|x| *x = 0)
        .count()
}

pub fn solve(input: String) -> (usize, usize) {
    let mut grid = [[0; 10]; 10];
    for (y, row) in input.lines().enumerate() {
        for (x, lvl) in row.bytes().enumerate() {
            grid[y][x] = lvl as i8 - b'0' as i8;
        }
    }

    let part1 = (0..100).map(|_| step(&mut grid)).sum();

    let mut sync_step = 101;
    let goal = grid.width() * grid.height();
    while step(&mut grid) as usize != goal {
        sync_step += 1;
    }

    (part1, sync_step)
}
