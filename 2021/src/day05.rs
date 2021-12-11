//! Hydrothermal Venture

use itertools::Itertools;

type Line = (i32, i32, i32, i32);

fn parse_line(line: String) -> Line {
    line.split(" -> ")
        .map(|x| x.split(','))
        .flatten()
        .map(|x| x.parse().unwrap())
        .collect_tuple()
        .unwrap()
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

pub fn solve(input: Vec<String>) -> (usize, usize) {
    let (straight, diagonal) = input
        .into_iter()
        .map(parse_line)
        .partition(|(x1, y1, x2, y2)| x1 == x2 || y1 == y2);
    let mut overlap = [[0; 1000]; 1000];
    let part1 = add_lines(&mut overlap, straight);
    let part2 = part1 + add_lines(&mut overlap, diagonal);
    (part1, part2)
}
