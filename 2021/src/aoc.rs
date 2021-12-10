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

pub trait AocNumber {}

macro_rules! impl_aoc_number {
    ($($ty:ty),*) => {
        $(impl AocNumber for $ty {})*
    };
}

impl_aoc_number!(u8, u16, u32, u64, i8, i16, i32, i64, usize, isize);

impl<T> AocInput for Vec<T>
where
    T: AocNumber,
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

impl AocInput for Vec<String> {
    fn make(input: String) -> Vec<String> {
        input.lines().map(|x| x.to_string()).collect()
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
pub trait Grid<T> {
    fn getyx(&self, y: usize, x: usize) -> Option<&T>;
    fn getyx_mut(&mut self, y: usize, x: usize) -> Option<&mut T>;
}

impl<T> Grid<T> for Vec<Vec<T>> {
    #[inline(always)]
    fn getyx(&self, y: usize, x: usize) -> Option<&T> {
        self.get(y).and_then(|row| row.get(x))
    }

    #[inline(always)]
    fn getyx_mut(&mut self, y: usize, x: usize) -> Option<&mut T> {
        self.get_mut(y).and_then(|row| row.get_mut(x))
    }
}
