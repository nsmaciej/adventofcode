//! Advent of Code utilities.

pub use itertools::Itertools;

use std::fmt::{Debug, Display};
use std::str::FromStr;

pub trait AocInput {
    fn make(input: String) -> Self
    where
        Self: Sized;
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

pub fn run<T, A, B>(solution: impl Fn(T) -> (A, B), input: String) -> (String, String)
where
    T: AocInput,
    A: Display,
    B: Display,
{
    let (a, b) = solution(T::make(input));
    (a.to_string(), b.to_string())
}

pub fn numbers<T>(line: &str, sep: char) -> impl Iterator<Item = T> + '_
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    line.split(sep).map(|x| x.parse::<T>().unwrap())
}
