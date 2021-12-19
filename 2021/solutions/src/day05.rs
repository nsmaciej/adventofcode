//! Hydrothermal Venture

type Line = (i32, i32, i32, i32);

fn parse_line(line: &str) -> Line {
    let (lhs, rhs) = line.split_once(" -> ").unwrap();
    let (x1, y1) = lhs.split_once(',').unwrap();
    let (x2, y2) = rhs.split_once(',').unwrap();
    (
        x1.parse().unwrap(),
        y1.parse().unwrap(),
        x2.parse().unwrap(),
        y2.parse().unwrap(),
    )
}

fn add_lines(overlap: &mut [[u8; 1000]; 1000], lines: Vec<Line>) -> usize {
    let mut result = 0;
    for (mut x1, mut y1, x2, y2) in lines {
        let dx = (x2 - x1).signum();
        let dy = (y2 - y1).signum();
        overlap[x1 as usize][y1 as usize] += 1;
        while x1 != x2 || y1 != y2 {
            x1 += dx;
            y1 += dy;
            if overlap[x1 as usize][y1 as usize] == 1 {
                result += 1;
            }
            overlap[x1 as usize][y1 as usize] += 1;
        }
    }
    result // How many new overlaps did we add.
}

pub fn solve(input: String) -> (usize, usize) {
    let (straight, diagonal) = input
        .lines()
        .map(parse_line)
        .partition(|(x1, y1, x2, y2)| x1 == x2 || y1 == y2);
    let mut overlap = [[0; 1000]; 1000];
    let part1 = add_lines(&mut overlap, straight);
    let part2 = part1 + add_lines(&mut overlap, diagonal);
    (part1, part2)
}
