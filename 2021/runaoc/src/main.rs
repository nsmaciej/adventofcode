#![feature(let_else)]

use owo_colors::OwoColorize;
use pico_args::Arguments;

use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::io::{self, prelude::*};
use std::time::{Duration, Instant};

use solutions::{run_day, Day, Solution, DAYS};

type Snapshots = BTreeMap<Day, (String, String)>;

fn get_snapshots() -> Result<Snapshots, Box<dyn Error>> {
    let mut snapshot = json::parse(&fs::read_to_string("snapshot.json")?)?;
    Ok(snapshot
        .members_mut()
        .map(|obj| {
            let day = obj["day"].as_u32().expect("day integer");
            let part1 = obj["part1"].take_string().expect("part 1 snapshot");
            let part2 = obj["part2"].take_string().expect("part 2 snapshot");
            (day, (part1, part2))
        })
        .collect())
}

fn compare_snapshot(snapshots: &Snapshots, day: Day, solution: &Solution) -> bool {
    let rule = "  \u{2502} ".red().bold().to_string();

    let show_diff = |part: u32, got: &str, expected: &str| -> bool {
        if got == expected {
            return false;
        }

        eprintln!();
        eprintln!(
            "{rule}{}",
            format!("Solution to day {day}, part {part}").red().bold()
        );
        eprintln!("{rule}");
        for line in got.lines() {
            eprintln!("{rule}{}", line);
        }
        eprintln!("{rule}");
        eprintln!("{rule}{}", "Does not match the snapshot".red().bold());
        eprintln!("{rule}");
        for line in expected.lines() {
            eprintln!("{rule}{}", line);
        }
        true
    };

    let Some((expected1, expected2)) = snapshots.get(&day) else {
        eprintln!("\n{rule}{}\n", format!("Missing snapshot for day {day}").red().bold());
        return false;
    };

    let part1 = show_diff(1, &solution.part1, expected1);
    let part2 = show_diff(2, &solution.part2, expected2);
    let failed = part1 || part2;
    if failed {
        eprintln!(); // Diffs start and end with a newline.
    }
    failed
}

fn day_input_path(day: Day) -> String {
    format!("inputs/day{day:02}.txt")
}

fn run_all(time: bool, check: bool) -> Result<(), Box<dyn Error>> {
    fn truncate(text: &str) -> &str {
        if text.contains('\n') {
            "(muti-line)"
        } else {
            text
        }
    }

    if time {
        eprintln!();
        eprintln!(
            "{:7} {:>9}  {:>8} {:>17}",
            "Task".bold(),
            "Time",
            "Part 1".dimmed(),
            "Part 2".dimmed(),
        );
        eprintln!("{}", "―".repeat(46));
    }

    let snapshots = if check { Some(get_snapshots()?) } else { None };
    let mut total = Duration::ZERO;

    for day in 1..=DAYS {
        let input = fs::read_to_string(day_input_path(day))?;
        let day_start = Instant::now();
        let solution = run_day(day, input);
        if time {
            let elapsed = day_start.elapsed();
            let title = format!("Day {:}", day);
            total += elapsed;
            eprintln!(
                "{:7} {:7} µs  {:>8} {:>17}",
                title.bold(),
                elapsed.as_micros(),
                truncate(&solution.part1).dimmed(),
                truncate(&solution.part2).dimmed(),
            );
        } else {
            println!("{}\n{}", solution.part1, solution.part2);
        }
        if let Some(snapshots) = &snapshots {
            compare_snapshot(snapshots, day, &solution);
        }
    }

    if time {
        eprintln!("{}", "―".repeat(46));
        eprintln!("{:7} {:7} µs\n", "Total".bold(), total.as_micros());
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = Arguments::from_env();
    if args.contains(["-h", "--help"]) {
        eprintln!("aoc [-h -t -s -c] [<day> <input>]");
        return Ok(());
    }
    let time = args.contains(["-t", "--time"]);
    let snapshot = args.contains(["-s", "--snapshot"]);
    let check = args.contains(["-c", "--check"]);

    if let Some(day) = args.opt_free_from_str()? {
        let arg_input_path = args.opt_free_from_str::<String>()?;
        let input = match arg_input_path.as_deref() {
            Some("-") => {
                let mut buf = String::new();
                io::stdin().read_to_string(&mut buf)?;
                buf
            }
            Some(path) => fs::read_to_string(path)?,
            None => fs::read_to_string(&day_input_path(day))?,
        };
        let start = Instant::now();
        let solution = run_day(day, input);
        let elapsed = start.elapsed();
        println!("{}\n{}", solution.part1, solution.part2);
        if time {
            eprintln!("{} {:.2?} µs", "Took".bold(), elapsed.as_micros());
        }
        // Don't check if we are using a custom input path.
        if check && arg_input_path.is_none() {
            compare_snapshot(&get_snapshots()?, day, &solution);
        }
    } else if snapshot {
        use json::*;
        let mut outputs = Vec::new();
        for day in 1..=DAYS {
            let input = fs::read_to_string(day_input_path(day))?;
            let solution = run_day(day, input);
            outputs.push(object! {
                "day": day,
                "part1": solution.part1,
                "part2": solution.part2
            });
        }
        println!("{}", stringify_pretty(outputs, 4));
    } else {
        run_all(time, check)?;
    }

    Ok(())
}
