//! Advent of Code utilities.

pub use itertools::Itertools;

use std::fmt::{Debug, Display};
use std::str::FromStr;

pub trait AocInput {
    fn make(input: String) -> Self
    where
        Self: Sized;
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

pub fn run<T, R>(solution: impl Fn(T) -> R, input: String)
where
    T: AocInput,
    R: AocOutput,
{
    solution(T::make(input)).show();
}

pub fn numbers<T>(line: &str, sep: char) -> impl Iterator<Item = T> + '_
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    line.split(sep).map(|x| x.parse::<T>().unwrap())
}
