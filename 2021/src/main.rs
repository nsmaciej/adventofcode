mod aoc;
mod day01;
mod day02;

use aoc::run;

fn main() {
    run(1, day01::solve);
    run(2, day02::solve);
}
