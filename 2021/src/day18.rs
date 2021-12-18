use arrayvec::ArrayVec;
use itertools::Itertools;

const PAIR: i32 = -1;

fn parse_sfn(line: &str) -> Vec<i32> {
    let mut r = Vec::new();
    for x in line.bytes() {
        match x {
            b'[' => r.push(PAIR),
            b'0'..=b'9' => r.push((x - b'0') as i32),
            b']' | b',' | b' ' => {}
            _ => panic!("unexpected character"),
        }
    }
    r
}

fn explode_sfn_at_index(sfn: &mut Vec<i32>, i: usize) {
    let a = sfn[i + 1];
    let b = sfn[i + 2];
    sfn.splice(i..i + 3, [0]);
    let mut j = i; // Left destination.
    while j > 0 {
        j -= 1;
        if sfn[j] != PAIR {
            sfn[j] += a;
            break;
        }
    }
    let mut j = i + 1; // Right destination.
    while j < sfn.len() {
        if sfn[j] != PAIR {
            sfn[j] += b;
            break;
        }
        j += 1;
    }
}

fn explode_sfn(sfn: &mut Vec<i32>) -> bool {
    let mut pairs = ArrayVec::<_, 5>::new();
    for i in 0..sfn.len() {
        if sfn[i] == PAIR {
            pairs.push(true);
            if pairs.len() > 4 {
                explode_sfn_at_index(sfn, i);
                return true;
            }
        } else {
            while let Some(false) = pairs.last() {
                pairs.pop();
            }
            pairs.last_mut().map(|x| *x = false);
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
    let mut pairs = ArrayVec::<_, 4>::new();
    let mut mags = ArrayVec::<_, 5>::new();
    for x in sfn {
        if *x == PAIR {
            pairs.push(true);
        } else {
            mags.push(*x);
            while let Some(false) = pairs.last() {
                let b = mags.pop().unwrap();
                let a = mags.pop().unwrap();
                mags.push(2 * b + 3 * a);
                pairs.pop();
            }
            pairs.last_mut().map(|x| *x = false);
        }
    }
    mags.pop().unwrap()
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
