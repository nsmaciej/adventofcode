use itertools::Itertools;

const PAIR: i32 = -1;
const UNPAIR: i32 = -2;

fn parse_sfn(line: &str) -> Vec<i32> {
    line.bytes()
        .flat_map(|x| match x {
            b'[' => Some(PAIR),
            b']' => Some(UNPAIR),
            b'0'..=b'9' => Some((x - b'0') as i32),
            b',' | b' ' => None,
            _ => panic!("unexpected character"),
        })
        .collect()
}

fn explode_sfn(sfn: &mut Vec<i32>) -> bool {
    let mut depth = 0;
    for i in 0..sfn.len() {
        if sfn[i] == PAIR && depth >= 4 {
            let a = sfn[i + 1];
            let b = sfn[i + 2];
            sfn.splice(i..i + 4, [0]);
            if let Some(x) = sfn[..i].iter_mut().rev().find(|x| **x >= 0) {
                *x += a;
            }
            if let Some(x) = sfn[i + 1..].iter_mut().find(|x| **x >= 0) {
                *x += b;
            }
            return true;
        } else if sfn[i] == PAIR {
            depth += 1;
        } else if sfn[i] == UNPAIR {
            depth -= 1;
        }
    }
    false
}

fn split_sfn(sfn: &mut Vec<i32>) -> bool {
    if let Some((i, x)) = sfn.iter().cloned().find_position(|x| *x >= 10) {
        sfn.splice(i..i + 1, [PAIR, x / 2, (x + 1) / 2, UNPAIR]);
        return true;
    }
    false
}

fn add_sfn(mut dest: Vec<i32>, rhs: &[i32]) -> Vec<i32> {
    dest.reserve(2 + rhs.len());
    dest.insert(0, PAIR);
    dest.extend(rhs.iter().cloned().chain([UNPAIR]));
    while explode_sfn(&mut dest) || split_sfn(&mut dest) {}
    dest
}

fn magnitude_sfn(sfn: &[i32]) -> i32 {
    let mut mags = [0; 5];
    let mut i = 0;
    for x in sfn {
        if *x == UNPAIR {
            mags[i - 2] = 3 * mags[i - 2] + 2 * mags[i - 1];
            i -= 1;
            continue;
        } else if *x >= 0 {
            mags[i] = *x;
            i += 1;
        }
    }
    mags[i - 1]
}

pub fn solve(input: String) -> (i32, i32) {
    let nums: Vec<Vec<i32>> = input.lines().map(parse_sfn).collect();

    let part1 = magnitude_sfn(&nums.iter().fold(nums[0].clone(), |a, x| add_sfn(a, x)));
    let part2 = nums
        .iter()
        .permutations(2)
        .map(|xs| magnitude_sfn(&add_sfn(xs[0].clone(), &xs[1])))
        .max()
        .unwrap();

    (part1, part2)
}
