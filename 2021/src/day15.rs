use std::collections::VecDeque;

use itertools::Itertools;

use crate::utils::Grid;

fn find_path(grid: &Vec<Vec<u8>>) -> u32 {
    let mut best = vec![vec![u32::MAX; grid.width()]; grid.height()];
    let mut queue = VecDeque::new();
    queue.push_back((0, 0, 0));

    let mut part1 = u32::MAX;
    while let Some((y, x, lvl)) = queue.pop_front() {
        let Some(add_lvl) = grid.getyx(y, x) else { continue };
        let Some(last_best) = best.getyx_mut(y, x) else { continue };
        let new_lvl = lvl + *add_lvl as u32;
        if *last_best <= new_lvl {
            continue;
        }
        *last_best = new_lvl;

        if y == grid.height() - 1 && x == grid.width() - 1 {
            part1 = part1.min(new_lvl);
        } else {
            queue.push_back((y, x + 1, new_lvl));
            queue.push_back((y + 1, x, new_lvl));
            queue.push_back((y.overflowing_sub(1).0, x, new_lvl));
            queue.push_back((y, x.overflowing_sub(1).0, new_lvl));
        }
    }

    part1 - grid[0][0] as u32
}

pub fn solve(input: String) -> (u32, u32) {
    let grid1 = input
        .lines()
        .map(|x| x.bytes().map(|x| x - b'0').collect_vec())
        .collect_vec();
    let part1 = find_path(&grid1);

    let mut grid2 = vec![vec![0; grid1.width() * 5]; grid1.height() * 5];
    for y in 0..grid2.height() {
        for x in 0..grid2.height() {
            let diff = (y / grid1.height() + x / grid1.width()) as u8;
            let v = grid1[y % grid1.height()][x % grid1.width()] + diff;
            grid2[y][x] = if v < 10 { v } else { 1 + (v % 10) };
        }
    }
    let part2 = find_path(&grid2);

    (part1, part2)
}
