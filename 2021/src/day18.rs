use itertools::Itertools;

const PAIR: i32 = -1;

fn parse_sfn(line: &str) -> Vec<i32> {
    line.bytes()
        .flat_map(|x| match x {
            b'[' => Some(PAIR),
            b'0'..=b'9' => Some((x - b'0') as i32),
            b']' | b',' | b' ' => None,
            _ => panic!("unexpected character"),
        })
        .collect()
}

fn explode_sfn(sfn: &mut Vec<i32>) -> bool {
    let mut pairs: u8 = 1;
    for i in 0..sfn.len() {
        if sfn[i] != PAIR {
            pairs = pairs >> pairs.trailing_zeros() & 0xfe;
            continue;
        }
        pairs = pairs << 1 | 1;
        if pairs.leading_zeros() == 2 {
            let a = sfn[i + 1];
            let b = sfn[i + 2];
            sfn.splice(i..i + 3, [0]);
            if let Some(x) = sfn[..i].iter_mut().rev().find(|x| **x != PAIR) {
                *x += a;
            }
            if let Some(x) = sfn[i + 1..].iter_mut().find(|x| **x != PAIR) {
                *x += b;
            }
            return true;
        }
    }
    false
}

fn split_sfn(sfn: &mut Vec<i32>) -> bool {
    for i in 0..sfn.len() {
        if sfn[i] >= 10 {
            sfn.splice(i..i + 1, [PAIR, sfn[i] / 2, (sfn[i] + 1) / 2]);
            return true;
        }
    }
    false
}

fn add_sfn(mut dest: Vec<i32>, rhs: &[i32]) -> Vec<i32> {
    dest.reserve(1 + rhs.len());
    dest.insert(0, PAIR);
    dest.extend(rhs);
    while explode_sfn(&mut dest) || split_sfn(&mut dest) {}
    dest
}

fn magnitude_sfn(sfn: &[i32]) -> i32 {
    let mut pairs: u8 = 1;
    let mut mags = [0; 5];
    let mut i = 0;
    for x in sfn {
        if *x == PAIR {
            pairs = pairs << 1 | 1;
            continue;
        }
        mags[i] = *x;
        i += 1;
        while pairs & 1 == 0 {
            mags[i - 2] = 3 * mags[i - 2] + 2 * mags[i - 1];
            pairs >>= 1;
            i -= 1;
        }
        pairs &= 0xfe;
    }
    mags[i - 1]
}

pub fn solve(input: String) -> (i32, i32) {
    let nums: Vec<Vec<i32>> = input.lines().map(parse_sfn).collect();

    let part1 = magnitude_sfn(&nums.iter().fold(nums[0].clone(), |a, x| add_sfn(a, x)));
    let part2 = nums
        .iter()
        .tuple_combinations()
        .map(|(x, y)| magnitude_sfn(&add_sfn(x.clone(), y)))
        .max()
        .unwrap();

    (part1, part2)
}
