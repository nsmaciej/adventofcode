use crate::utils::Grid;
use itertools::Itertools;
use std::collections::BinaryHeap;

fn find_path(grid: &mut Vec<Vec<u8>>) -> i32 {
    let start_lvl = grid[0][0] as i32;
    let mut heap = BinaryHeap::new();
    heap.push((0, 0, 0));

    while let Some((lvl, y, x)) = heap.pop() {
        let Some(grid_cell) = grid.getyx_mut(y, x) else { continue };

        if *grid_cell == 0 {
            continue;
        }
        let new_lvl = lvl - *grid_cell as i32;
        *grid_cell = 0;

        if y == grid.height() - 1 && x == grid.width() - 1 {
            return -start_lvl - new_lvl; // Flip from negatives.
        } else {
            heap.push((new_lvl, y, x + 1));
            heap.push((new_lvl, y + 1, x));
            heap.push((new_lvl, y.overflowing_sub(1).0, x));
            heap.push((new_lvl, y, x.overflowing_sub(1).0));
        }
    }
    panic!("no path found");
}

pub fn solve(input: String) -> (i32, i32) {
    let mut grid1 = input
        .lines()
        .map(|x| x.bytes().map(|x| x - b'0').collect_vec())
        .collect_vec();

    let mut grid2 = vec![vec![0; grid1.width() * 5]; grid1.height() * 5];
    for y in 0..grid2.height() {
        for x in 0..grid2.height() {
            let diff = (y / grid1.height() + x / grid1.width()) as u8;
            let v = grid1[y % grid1.height()][x % grid1.width()] + diff;
            grid2[y][x] = if v < 10 { v } else { 1 + (v % 10) };
        }
    }

    (find_path(&mut grid1), find_path(&mut grid2))
}
