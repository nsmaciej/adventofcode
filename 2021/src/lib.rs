#![feature(let_else)]
#![feature(array_windows)]

use std::fmt::Display;
use utils::AocInput;
use wasm_bindgen::prelude::*;

pub type Day = u32;

pub const DAYS: Day = 16;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;

mod utils;

#[wasm_bindgen]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Solution(String, String);

#[wasm_bindgen]
impl Solution {
    #[wasm_bindgen(getter)]
    pub fn part1(&self) -> String {
        self.0.clone()
    }
    #[wasm_bindgen(getter)]
    pub fn part2(&self) -> String {
        self.1.clone()
    }
}

#[wasm_bindgen]
pub fn run_day(day: Day, mut input: String) -> Solution {
    let trimmed_len = input.trim_end().len();
    input.truncate(trimmed_len);
    match day {
        // Do not forget to update the DAYS constant too.
        1 => run(day01::solve, input),
        2 => run(day02::solve, input),
        3 => run(day03::solve, input),
        4 => run(day04::solve, input),
        5 => run(day05::solve, input),
        6 => run(day06::solve, input),
        7 => run(day07::solve, input),
        8 => run(day08::solve, input),
        9 => run(day09::solve, input),
        10 => run(day10::solve, input),
        11 => run(day11::solve, input),
        12 => run(day12::solve, input),
        13 => run(day13::solve, input),
        14 => run(day14::solve, input),
        15 => run(day15::solve, input),
        16 => run(day16::solve, input),
        _ => panic!("day not implemented"),
    }
}

fn run<T, A, B>(solution: impl Fn(T) -> (A, B), input: String) -> Solution
where
    T: AocInput,
    A: Display,
    B: Display,
{
    let (a, b) = solution(T::make(input));
    Solution(a.to_string(), b.to_string())
}
