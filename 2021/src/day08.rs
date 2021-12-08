use std::collections::HashMap;

use crate::aoc::*;

fn segments() -> [u8; 10] {
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
    ]
}

fn assume<'a, 'b: 'a>(
    segments: &[u8; 10],
    cands: &mut Vec<Vec<u8>>,
    n: u8,
    pat: &'b str,
    assoc: &'a mut HashMap<&'b str, u8>,
) -> bool {
    for k in 0..8 {
        if (segments[n as usize].reverse_bits() >> k) & 1 > 0 {
            cands[k].retain(|x| pat.as_bytes().contains(&(x + 'a' as u8)));
            if cands[k].len() == 0 {
                return false;
            }
        }
    }
    assoc.insert(pat, n);
    true
}

pub fn solve(input: Vec<String>) -> (usize, u32) {
    let mut part1 = 0;
    let mut part2 = 0;
    let segments = segments();
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

        let mut candidate_orig = vec![vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]; 8];
        let mut assoc: HashMap<&str, u8> = HashMap::new();
        assume(&segments, &mut candidate_orig, 1, &pats[0], &mut assoc); // 2 on
        assume(&segments, &mut candidate_orig, 7, &pats[1], &mut assoc); // 3 on
        assume(&segments, &mut candidate_orig, 4, &pats[2], &mut assoc); // 4 on
        assume(&segments, &mut candidate_orig, 8, &pats[9], &mut assoc); // 8 on

        // Which digits can go in this segment.
        'search: for (fives, sixes) in [2, 3, 5]
            .iter()
            .permutations(3)
            .cartesian_product([0, 6, 9].iter().permutations(3))
        {
            let mut candidates = candidate_orig.clone();
            for (i, five) in fives.iter().enumerate() {
                if !assume(&segments, &mut candidates, **five, &pats[i + 3], &mut assoc) {
                    continue 'search;
                }
            }
            for (i, six) in sixes.iter().enumerate() {
                if !assume(&segments, &mut candidates, **six, &pats[i + 6], &mut assoc) {
                    continue 'search;
                }
            }
            let mut res: u32 = 0;
            for out in outputs {
                res *= 10;
                res += assoc[out.as_str()] as u32;
            }
            part2 += res;
            break;
        }
    }

    (part1, part2)
}
