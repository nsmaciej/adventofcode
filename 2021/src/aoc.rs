pub use itertools::Itertools;

use std::fmt::{Debug, Display};
use std::fs;
use std::str::FromStr;

pub trait AocInput {
    fn make(input: String) -> Self;
}

pub trait AocOutput {
    fn show(self);
}

impl AocInput for String {
    fn make(input: String) -> String {
        input
    }
}

impl<T> AocInput for Vec<T>
where
    T: FromStr,
    T::Err: Debug,
{
    fn make(input: String) -> Vec<T> {
        input
            .lines()
            .map(|x| x.parse().expect("parse failed"))
            .collect()
    }
}

impl AocOutput for () {
    fn show(self) {}
}

impl<A: Display> AocOutput for (A,) {
    fn show(self) {
        println!("{}", self.0);
    }
}

impl<A: Display, B: Display> AocOutput for (A, B) {
    fn show(self) {
        println!("{}", self.0);
        println!("{}", self.1);
    }
}

pub fn run<T, R>(day: u32, solution: impl Fn(T) -> R)
where
    T: AocInput,
    R: AocOutput,
{
    let input = fs::read_to_string(format!("inputs/day{day:02}.txt")).expect("input file");
    solution(T::make(input)).show();
}
