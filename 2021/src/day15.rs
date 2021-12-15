use crate::utils::Grid;
use itertools::Itertools;
use std::collections::BinaryHeap;

fn find_path(grid: &mut Vec<Vec<u8>>) -> i32 {
    let mut heap = BinaryHeap::new();
    let mut dist = vec![vec![i32::MAX; grid.width()]; grid.height()];
    heap.push((0, 0, 0));

    while let Some((cost, y, x)) = heap.pop() {
        if y == grid.height() - 1 && x == grid.width() - 1 {
            return -cost;
        }
        if -cost > dist[y][x] {
            continue;
        }

        for (y, x) in [
            (y, x.overflowing_sub(1).0),
            (y, x + 1),
            (y.overflowing_sub(1).0, x),
            (y + 1, x),
        ] {
            let Some(cell_cost) = grid.getyx(y, x) else { continue };
            let next_cost = -cost + *cell_cost as i32;
            if next_cost < dist[y][x] {
                dist[y][x] = next_cost;
                heap.push((-next_cost, y, x));
            }
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
