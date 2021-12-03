#![allow(dead_code)]
mod aoc;
mod day01;
mod day02;
mod day03;

use aoc::run;

fn main() {
    run(1, day01::solve);
    run(2, day02::solve);
    run(3, day03::solve);
}
