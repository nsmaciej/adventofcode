#![feature(let_else)]

use wasm_bindgen::prelude::*;

pub const DAYS: u32 = 11;

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

mod utils;

#[wasm_bindgen]
pub fn run_day_wasm(day: u32, input: String) -> String {
    let (a, b) = run_day(day, input);
    format!("{} {}", a, b)
}

pub fn run_day(day: u32, input: String) -> (String, String) {
    use utils::run;
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
        _ => panic!("day not implemented"),
    }
}
