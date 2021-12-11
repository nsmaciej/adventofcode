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

fn flood(grid: &mut Vec<Vec<i8>>, ys: usize, xs: usize) {
    for (y, x) in around(ys, xs) {
        let Some(cell) = grid.getyx_mut(y as usize, x as usize) else {
            continue; // Out of bounds.
        };
        if *cell == -1 {
            continue; // Already flashed.
        }
        *cell += 1;
        if *cell > 9 {
            *cell = -1;
            flood(grid, y as usize, x as usize);
        }
    }
}

fn step(grid: &mut Vec<Vec<i8>>) -> usize {
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
    let mut grid: Vec<Vec<i8>> = input
        .lines()
        .map(|line| line.bytes().map(|x| x as i8 - b'0' as i8).collect())
        .collect();

    let part1 = (0..100).map(|_| step(&mut grid)).sum();

    let mut sync_step = 101;
    let goal = grid.width() * grid.height();
    while step(&mut grid) as usize != goal {
        sync_step += 1;
    }

    (part1, sync_step)
}
