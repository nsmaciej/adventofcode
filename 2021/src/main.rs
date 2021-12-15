#![doc = include_str!("../README.md")]
#![allow(dead_code)]

use owo_colors::OwoColorize;
use pico_args::Arguments;

use std::error::Error;
use std::fs::read_to_string;
use std::io::{self, prelude::*};
use std::time::{Duration, Instant};

use aoclib::{run_day, DAYS};

fn day_input_path(day: u32) -> String {
    format!("inputs/day{day:02}.txt")
}

fn read_stdin() -> io::Result<String> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    Ok(buf)
}

fn truncate(text: &str) -> &str {
    if text.contains("\n") {
        "(muti-line)"
    } else {
        text
    }
}

fn run_all(time: bool) -> Result<(), Box<dyn Error>> {
    if time {
        eprintln!();
        eprintln!(
            "{:7} {:>9}  {:>10} {:>14}",
            "Task".bold(),
            "Time",
            "Part 1".dimmed(),
            "Part 2".dimmed(),
        );
        eprintln!("{}", "―".repeat(44));
    }
    let mut total = Duration::ZERO;
    for day in 1..=DAYS {
        let input = read_to_string(day_input_path(day))?;
        let day_start = Instant::now();
        let solution = run_day(day, input);
        if time {
            let desc = format!("Day {:}", day);
            let elapsed = day_start.elapsed();
            total += elapsed;
            eprintln!(
                "{:7} {:6} µs  {:>10} {:>14}",
                desc.bold(),
                elapsed.as_micros(),
                truncate(&solution.part1()).dimmed(),
                truncate(&solution.part2()).dimmed(),
            );
        } else {
            println!("{}\n{}", solution.part1(), solution.part2());
        }
    }
    if time {
        eprintln!("{}", "―".repeat(44));
        eprintln!("{:7} {:6} µs\n", "Total".bold(), total.as_micros());
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = Arguments::from_env();
    if args.contains(["-h", "--help"]) {
        eprintln!("aoc [-h -t -s] [<day> <input>]");
        return Ok(());
    }
    let time = args.contains(["-t", "--time"]);
    let snapshot = args.contains(["-s", "--snapshot"]);

    if let Some(day) = args.opt_free_from_str()? {
        let input = match args.opt_free_from_str::<String>()?.as_deref() {
            Some("-") => read_stdin()?,
            Some(path) => read_to_string(path)?,
            None => read_to_string(&day_input_path(day))?,
        };
        let start = Instant::now();
        let solution = run_day(day, input);
        println!("{}\n{}", solution.part1(), solution.part2());
        if time {
            eprintln!("{} in {:.2?}", "Finished".bold(), start.elapsed());
        }
    } else if snapshot {
        use json::*;
        let mut outputs = Vec::new();
        for day in 1..=DAYS {
            let input = read_to_string(day_input_path(day))?;
            let solution = run_day(day, input);
            outputs.push(object! {
                "day": day,
                "part1": solution.part1(),
                "part2": solution.part2()
            });
        }
        println!("{}", stringify_pretty(outputs, 4));
    } else {
        run_all(time)?;
    }

    Ok(())
}
