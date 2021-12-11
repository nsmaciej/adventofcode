//! Smoke Basin

// N.B. this code relies on `overflowing_sub` to produce out of bounds positive
// indices in the grid. It also doesn't bother parsing the digits, relying on
// ascii values instead. To avoid dupliate bounds checks without resorting to
// unsafe code it bundles the `visited` and `grid` vectors together, and uses
// `Vec::get()` whenever possible.

use crate::aoc::Grid;

fn flood(grid: &mut Vec<Vec<(u8, bool)>>, y: usize, x: usize) -> i32 {
    if let Some((height, visited)) = grid.getyx_mut(y, x) {
        if *visited || *height == b'9' {
            0
        } else {
            *visited = true;
            1 + flood(grid, y, x.overflowing_sub(1).0)
                + flood(grid, y, x + 1)
                + flood(grid, y.overflowing_sub(1).0, x)
                + flood(grid, y + 1, x)
        }
    } else {
        0
    }
}

pub fn solve(input: String) -> (i32, i32) {
    let mut grid: Vec<Vec<(u8, bool)>> = input
        .lines()
        .map(|line| line.bytes().map(|x| (x, false)).collect())
        .collect();

    let mut sum_risk = 0;
    let mut basins = Vec::new();

    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            let mut lowest = u8::MAX;
            if let Some(cell) = grid.getyx(y + 1, x) {
                lowest = lowest.min(cell.0);
            }
            if let Some(cell) = grid.getyx(y, x + 1) {
                lowest = lowest.min(cell.0);
            }
            if let Some(cell) = grid.getyx(y.overflowing_sub(1).0, x) {
                lowest = lowest.min(cell.0);
            }
            if let Some(cell) = grid.getyx(y, x.overflowing_sub(1).0) {
                lowest = lowest.min(cell.0);
            }
            if grid[y][x].0 < lowest {
                sum_risk += (grid[y][x].0 - b'0' + 1) as i32;
                basins.push(flood(&mut grid, y, x));
            }
        }
    }

    let (ab, c, _) = basins.select_nth_unstable_by(2, |a, b| b.cmp(a));
    (sum_risk, ab[0] * ab[1] * *c)
}
