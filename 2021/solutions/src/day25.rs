const WIDTH: usize = 139;
const HEIGHT: usize = 137;
type Seafloor = [[u8; WIDTH]; HEIGHT];

fn step(grid: &mut Seafloor) -> bool {
    let mut moved = 0;

    // Move east.
    for y in 0..HEIGHT {
        let mut offset = 0;
        if grid[y][WIDTH - 1] == b'>' && grid[y][0] == b'.' {
            grid[y][WIDTH - 1] = b'.';
            grid[y][0] = b'>';
            offset = 1;
            moved += 1;
        }
        let mut x = offset;
        while x < WIDTH - offset - 1 as usize {
            if grid[y][x] == b'>' && grid[y][x + 1] == b'.' {
                grid[y][x + 1] = b'>';
                grid[y][x] = b'.';
                x += 1;
                moved += 1;
            }
            x += 1;
        }
    }

    // Move south.
    for x in 0..WIDTH {
        let mut offset = 0;
        if grid[HEIGHT - 1][x] == b'v' && grid[0][x] == b'.' {
            grid[HEIGHT - 1][x] = b'.';
            grid[0][x] = b'v';
            moved += 1;
            offset = 1;
        }
        let mut y = offset;
        while y < HEIGHT - offset - 1 {
            if grid[y][x] == b'v' && grid[y + 1][x] == b'.' {
                grid[y + 1][x] = b'v';
                grid[y][x] = b'.';
                y += 1;
                moved += 1;
            }
            y += 1;
        }
    }

    println!("{moved}");
    moved > 0
}

pub fn solve(input: String) -> (i32, &'static str) {
    let mut input = input
        .lines()
        .map(|x| x.as_bytes().try_into().unwrap())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();
    let mut turns = 0;
    while step(&mut input) {
        turns += 1;
    }
    (turns + 1, "Singal Boosted")
}
