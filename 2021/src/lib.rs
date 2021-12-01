pub use itertools::Itertools;
pub use std::error::Error;

use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};

pub fn input_lines(day: u32) -> io::Result<io::Lines<BufReader<File>>> {
    let file = File::open(format!("inputs/day{day:02}.txt")).expect("no input file found");
    let reader = BufReader::new(file);
    Ok(reader.lines())
}
