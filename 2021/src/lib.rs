pub use itertools::Itertools;
pub use std::error::Error;

use std::borrow::Borrow;
use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};

pub type Main = Result<(), Box<dyn Error>>;

pub fn lines(day: u32) -> io::Result<Vec<String>> {
    let file = File::open(format!("inputs/day{day:02}.txt"))?;
    let reader = BufReader::new(file);
    reader.lines().collect()
}

pub fn puts<T: Display, R: Borrow<T>>(data: R) {
    println!("{}", data.borrow());
}
