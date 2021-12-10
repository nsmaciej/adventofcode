use std::collections::HashMap;

use crate::aoc::*;

fn segments(n: u8) -> u8 {
    [
        //abcdefg-
        0b11101110, // 0
        0b00100100, // 1
        0b10111010, // 2
        0b10110100, // 3
        0b01110100, // 4
        0b11010110, // 5
        0b11011110, // 6
        0b10100100, // 7
        0b11111110, // 8
        0b11110110, // 9
    ][n as usize]
}

#[derive(Clone, Debug)]
struct Search<'a> {
    pattern_digit: HashMap<&'a str, u8>,
    segment_candidates: Vec<Vec<u8>>,
}

impl<'a> Search<'a> {
    fn new() -> Search<'a> {
        Search {
            pattern_digit: HashMap::new(),
            segment_candidates: vec![(0..8).collect(); 8],
        }
    }

    fn assume<'b: 'a>(&mut self, n: u8, pat: &'b str) -> bool {
        for k in 0..8 {
            if (segments(n) >> (7 - k)) & 1 > 0 {
                self.segment_candidates[k].retain(|x| pat.as_bytes().contains(&(x + 'a' as u8)));
                if self.segment_candidates[k].len() == 0 {
                    return false;
                }
            }
        }
        self.pattern_digit.insert(pat, n);
        true
    }
}

pub fn solve(input: Vec<String>) -> (usize, u32) {
    let mut part1 = 0;
    let mut part2 = 0;
    for line in input {
        let (patterns, outputs) = line.split(" | ").collect_tuple().unwrap();
        let outputs = outputs
            .split_whitespace()
            .map(|x| x.chars().sorted().collect::<String>())
            .collect_vec();
        let pats = patterns
            .split_whitespace()
            .map(|x| x.chars().sorted().collect::<String>())
            .sorted_by_key(|x| x.len())
            .collect_vec();

        part1 += outputs
            .iter()
            .map(|x| x.len())
            .filter(|x| *x == 2 || *x == 4 || *x == 3 || *x == 7)
            .count();

        let mut search = Search::new();
        search.assume(1, &pats[0]); // 2 on
        search.assume(7, &pats[1]); // 3 on
        search.assume(4, &pats[2]); // 4 on
        search.assume(8, &pats[9]); // 8 on

        'search: for (fives, sixes) in [2, 3, 5]
            .iter()
            .permutations(3)
            .cartesian_product([0, 6, 9].iter().permutations(3))
        {
            let mut sub_search = search.clone();
            for (i, digit) in fives.iter().chain(sixes.iter()).enumerate() {
                if !sub_search.assume(**digit, &pats[i + 3]) {
                    continue 'search;
                }
            }
            let mut res: u32 = 0;
            for out in outputs {
                res *= 10;
                res += sub_search.pattern_digit[out.as_str()] as u32;
            }
            part2 += res;
            break;
        }
    }

    (part1, part2)
}
