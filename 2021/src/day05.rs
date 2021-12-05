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
    let (lhs, rhs) = line.split(" -> ").collect_tuple().unwrap();
    Line {
        start: parse_point(lhs),
        end: parse_point(rhs),
    }
}

fn sign(n: i32) -> i32 {
    (n > 0) as i32 - (n < 0) as i32
}

pub fn solve(input: Vec<String>) -> (usize, usize) {
    let lines = input.into_iter().map(|x| parse_line(x)).collect_vec();
    let mut overlap1: HashMap<Point, i32> = HashMap::new();
    let mut overlap2: HashMap<Point, i32> = HashMap::new();

    for line in lines {
        let straight = line.start.0 == line.end.0 || line.start.1 == line.end.1;
        let dx = sign(line.end.0 - line.start.0);
        let dy = sign(line.end.1 - line.start.1);
        let mut start = line.start;
        *overlap2.entry(start).or_insert(0) += 1;
        if straight {
            *overlap1.entry(start).or_insert(0) += 1;
        }
        while start != line.end {
            start.0 += dx;
            start.1 += dy;
            *overlap2.entry(start).or_insert(0) += 1;
            if straight {
                *overlap1.entry(start).or_insert(0) += 1;
            }
        }
    }
    let part1 = overlap1.values().filter(|x| **x >= 2).count();
    let part2 = overlap2.values().filter(|x| **x >= 2).count();
    (part1, part2)
}
