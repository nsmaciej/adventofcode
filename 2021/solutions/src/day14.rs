//! Extended Polymerization

use std::{collections::BTreeMap, mem};

#[inline]
fn key(a: u8, b: u8) -> u16 {
    (b as u16) << 8 | a as u16
}

#[inline]
fn unkey(ab: u16) -> (u8, u8) {
    (ab as u8, (ab >> 8) as u8)
}

fn simulate(
    k: usize,
    first: u8,
    last: u8,
    pairs_orig: &mut BTreeMap<u16, u64>,
    rules: &BTreeMap<u16, u8>,
) -> u64 {
    let mut pairs = pairs_orig.clone();
    for _ in 0..k {
        let mut next = BTreeMap::new();
        for (ab, k) in &mut pairs {
            let (a, b) = unkey(*ab);
            let result = rules[ab];
            *next.entry(key(a, result)).or_default() += *k;
            *next.entry(key(result, b)).or_default() += *k;
            *k = 0;
        }
        mem::swap(&mut pairs, &mut next);
    }

    let mut counts: BTreeMap<u8, u64> = BTreeMap::new();
    for (ab, k) in &pairs {
        let (a, b) = unkey(*ab);
        *counts.entry(a).or_default() += k;
        *counts.entry(b).or_default() += k;
    }

    let (most_el, most_k) = counts.iter().max_by_key(|x| x.1).unwrap();
    let (least_el, least_k) = counts.iter().min_by_key(|x| x.1).unwrap();
    let most = (1 + (last == *most_el) as u64 + (first == *most_el) as u64 + most_k) / 2;
    let least = (1 + (last == *least_el) as u64 + (first == *least_el) as u64 + least_k) / 2;
    *pairs_orig = pairs;
    most - least
}

pub fn solve(input: String) -> (u64, u64) {
    let mut lines = input.lines();
    let mut pairs: BTreeMap<u16, u64> = BTreeMap::new();
    let template: Vec<u8> = lines.next().unwrap().bytes().collect();
    let first = *template.first().unwrap();
    let last = *template.last().unwrap();
    for (a, b) in template.iter().zip(template.iter().skip(1)) {
        *pairs.entry(key(*a, *b)).or_default() += 1;
    }
    lines.next(); // Skip the blank line.
    let rules: BTreeMap<u16, u8> = lines
        .map(|rule| {
            let (pair, result) = rule.split_once(" -> ").unwrap();
            let pair = pair.as_bytes();
            (key(pair[0], pair[1]), result.bytes().next().unwrap())
        })
        .collect();

    (
        simulate(10, first, last, &mut pairs, &rules),
        simulate(30, first, last, &mut pairs, &rules),
    )
}
