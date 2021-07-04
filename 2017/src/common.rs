#![allow(dead_code)]

use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::ops::{Add, AddAssign, Index};

pub use std::collections::*;

fn open_aoc_file(day: u32) -> File {
    let path = format!("input/day{}.txt", day);
    File::open(&path).expect("could not open input")
}

/// More efficient way to read the input line by line.
pub fn aoc_input_lines(day: u32) -> Vec<String> {
    let buffer = BufReader::new(open_aoc_file(day));
    buffer.lines().map(|x| x.unwrap()).collect()
}

pub fn aoc_input(day: u32) -> String {
    let mut buffer = String::new();
    open_aoc_file(day)
        .read_to_string(&mut buffer)
        .expect("could not read input");
    buffer
}

#[derive(Debug, Clone, Copy)]
pub struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Point;
    fn add(self, rhs: Self) -> Point {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl Point {
    pub fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    pub fn turn_right(self) -> Point {
        Point {
            x: -self.y,
            y: self.x,
        }
    }

    pub fn turn_left(self) -> Point {
        Point {
            x: self.y,
            y: -self.x,
        }
    }
}

impl<T> Index<Point> for Vec<Vec<T>> {
    type Output = T;
    fn index(&self, point: Point) -> &T {
        &self[point.y as usize][point.x as usize]
    }
}
