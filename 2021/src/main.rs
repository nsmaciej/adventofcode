//! Advent of Code 2021 solutions by Maciej Goszczycki.

#![allow(dead_code)]

mod aoc;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;

use pico_args::Arguments;
use std::error::Error;
use std::fs::read_to_string;
use std::io::{self, prelude::*};
use std::time::Instant;

const DAYS: u32 = 5;

fn run_day(day: u32, input: String) {
    use aoc::run;

    match day {
        1 => run(day01::solve, input),
        2 => run(day02::solve, input),
        3 => run(day03::solve, input),
        4 => run(day04::solve, input),
        5 => run(day05::solve, input),
        _ => panic!("day not implemented"),
    }
}

fn day_input_path(day: u32) -> String {
    format!("inputs/day{day:02}.txt")
}

fn read_stdin() -> io::Result<String> {
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf)?;
    Ok(buf)
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = Arguments::from_env();
    if args.contains(["-h", "--help"]) {
        eprintln!("aoc [-h -t -p] [<day> <input>]");
        return Ok(());
    }
    let time = args.contains(["-t", "--time"]);
    let pretty = args.contains(["-p", "--pretty"]);

    let start = Instant::now();

    if let Some(day) = args.opt_free_from_str()? {
        let input = match args.opt_free_from_str::<String>()?.as_deref() {
            Some("-") => read_stdin()?,
            Some(path) => read_to_string(path)?,
            None => read_to_string(&day_input_path(day))?,
        };
        run_day(day, input);
    } else {
        for day in 1..=DAYS {
            let input = read_to_string(day_input_path(day))?;
            let day_start = Instant::now();
            run_day(day, input);
            if pretty {
                if time {
                    eprintln!("\x1b[3m↑ Day {} - {:.2?}\x1b[0m", day, day_start.elapsed());
                } else {
                    eprintln!("\x1b[3m↑ Day {}\x1b[0m", day);
                }
            }
        }
    }

    if time {
        eprintln!("\n\x1b[3m{:.2?}\x1b[0m", start.elapsed());
    }
    Ok(())
}
