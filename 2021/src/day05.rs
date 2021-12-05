use std::collections::HashMap;

use crate::aoc::*;

type Point = (i32, i32);

#[derive(Debug)]
struct Line {
    start: Point,
    end: Point,
}

fn parse_point(point: &str) -> Point {
    point
        .split(",")
        .map(|x| x.parse().unwrap())
        .collect_tuple()
        .unwrap()
}

fn parse_line(line: String) -> Line {
    let (start, end) = line.split(" -> ").map(parse_point).collect_tuple().unwrap();
    Line { start, end }
}

fn sign(n: i32) -> i32 {
    (n > 0) as i32 - (n < 0) as i32
}

fn add_lines(overlap: &mut HashMap<Point, i32>, lines: Vec<Line>) -> usize {
    for line in lines {
        *overlap.entry(line.start).or_default() += 1;
        let dx = sign(line.end.0 - line.start.0);
        let dy = sign(line.end.1 - line.start.1);
        let mut point = line.start;
        while point != line.end {
            point.0 += dx;
            point.1 += dy;
            *overlap.entry(point).or_default() += 1;
        }
    }
    overlap.values().filter(|x| **x >= 2).count()
}

pub fn solve(input: Vec<String>) -> (usize, usize) {
    let (straight, diagonal): (Vec<Line>, Vec<Line>) = input
        .into_iter()
        .map(|x| parse_line(x))
        .partition(|x| x.start.0 == x.end.0 || x.start.1 == x.end.1);

    let mut overlap: HashMap<Point, i32> = HashMap::new();
    (
        add_lines(&mut overlap, straight),
        add_lines(&mut overlap, diagonal),
    )
}
