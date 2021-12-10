use crate::aoc::around;

fn flood(grid: &[Vec<u8>], is_basin: &mut Vec<Vec<bool>>, y: usize, x: usize) -> i32 {
    if is_basin[y][x] || grid[y][x] == 9 {
        0
    } else {
        is_basin[y][x] = true;
        1 + around(grid.len(), grid[0].len(), y, x)
            .map(|(y, x)| flood(grid, is_basin, y, x))
            .sum::<i32>()
    }
}

pub fn solve(input: Vec<String>) -> (i32, i32) {
    let width = input[0].len();
    let height = input.len();
    let grid: Vec<Vec<u8>> = input
        .iter()
        .map(|line| {
            line.chars()
                .map(|x| x.to_digit(10).unwrap() as u8)
                .collect()
        })
        .collect();

    let mut sum_risk = 0;
    let mut is_basin = vec![vec![false; width]; height];
    let mut basins = Vec::new();
    for y in 0..height {
        for x in 0..width {
            let around = around(height, width, y, x)
                .map(|(y, x)| grid[y][x])
                .min()
                .unwrap();
            if grid[y][x] < around {
                sum_risk += grid[y][x] as i32 + 1;
                basins.push(flood(&grid, &mut is_basin, y, x));
            }
        }
    }

    basins.sort_by(|a, b| b.cmp(a));
    (sum_risk, basins[0] * basins[1] * basins[2])
}
