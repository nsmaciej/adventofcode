#![doc = include_str!("../README.md")]
#![allow(dead_code)]

use owo_colors::OwoColorize;
use pico_args::Arguments;

use std::error::Error;
use std::fs::read_to_string;
use std::io::{self, prelude::*};
use std::time::Instant;

use aoclib::{run_day, DAYS};

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
        let (a, b) = run_day(day, input);
        println!("{}\n{}", a, b);
    } else {
        for day in 1..=DAYS {
            let input = read_to_string(day_input_path(day))?;
            let day_start = Instant::now();
            let (a, b) = run_day(day, input);
            if pretty {
                let desc = format!("Day {}", day);
                if time {
                    eprintln!("{} took {:.2?}", desc.bold(), day_start.elapsed());
                } else {
                    eprintln!("{:12}", desc.bold());
                }
                let sep = '│'.dimmed();
                println!("   {} {}\n   {} {}\n", sep, a.dimmed(), sep, b.dimmed())
            } else {
                println!("{}\n{}", a, b);
            }
        }
    }

    if time {
        eprintln!("{} in {:.2?}", "Finished".bold(), start.elapsed());
    }
    Ok(())
}
