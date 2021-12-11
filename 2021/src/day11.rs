use std::collections::HashSet;

use crate::utils::Grid;

fn step(grid: &mut Vec<Vec<u8>>) -> i64 {
    let mut stack = Vec::new();
    let mut flashed = HashSet::new();

    for y in 0..grid.width() {
        for x in 0..grid.height() {
            grid[y][x] += 1;
            if grid[y][x] > 9 {
                stack.push((y, x));
            }
        }
    }

    while let Some((y, x)) = stack.pop() {
        if flashed.contains(&(y, x)) {
            continue;
        }
        flashed.insert((y, x));
        macro_rules! flash {
            ($y:expr, $x:expr) => {
                if let Some(cell) = grid.getyx_mut($y, $x) {
                    *cell += 1;
                    if *cell > 9 {
                        stack.push(($y, $x));
                    }
                }
            };
        }
        flash!(y + 1, x);
        flash!(y + 1, x + 1);
        flash!(y + 1, x.overflowing_sub(1).0);

        flash!(y, x + 1);
        flash!(y, x.overflowing_sub(1).0);

        flash!(y.overflowing_sub(1).0, x);
        flash!(y.overflowing_sub(1).0, x + 1);
        flash!(y.overflowing_sub(1).0, x.overflowing_sub(1).0);
    }

    for (y, x) in &flashed {
        grid[*y][*x] = 0;
    }

    flashed.len() as i64
}

pub fn solve(input: String) -> (i64, i64) {
    let mut grid: Vec<Vec<u8>> = input
        .lines()
        .map(|line| line.bytes().map(|x| x - b'0').collect())
        .collect();

    let mut total = 0;
    for _ in 0..100 {
        total += step(&mut grid);
    }

    let mut i = 0;
    loop {
        i += 1;
        if step(&mut grid) as usize == grid.width() * grid.height() {
            break;
        }
    }

    (total, 100 + i)
}
