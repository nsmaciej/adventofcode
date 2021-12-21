//! Dirac Dice

use ahash::AHashMap;
use itertools::Itertools;
use std::mem::swap;

fn part1(mut x: i32, mut y: i32) -> i32 {
    let mut x_score = 0;
    let mut y_score = 0;
    let mut rolls = 1;
    while y_score < 1000 {
        rolls += 3;
        let d = (rolls * 3) % 100;
        x = (x + d - 1) % 10 + 1;
        x_score += x;
        swap(&mut x, &mut y);
        swap(&mut x_score, &mut y_score);
    }
    x_score.min(y_score) * (rolls - 1)
}

// Sum of three rolls and how often it will happen per turn.
static OUTCOMES: [(i32, i64); 7] = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)];

type Cache = AHashMap<(i32, i32, i32, i32, bool), i64>;

fn part2(cache: &mut Cache, x: i32, x_score: i32, y: i32, y_score: i32, turn: bool) -> i64 {
    if let Some(r) = cache.get(&(x, x_score, y, y_score, turn)) {
        return *r;
    }
    let r = if x_score >= 21 {
        return 1;
    } else if y_score >= 21 {
        return 0;
    } else {
        let mut r = 0;
        for (roll_sum, freq) in OUTCOMES {
            if turn {
                let next_x = (x + roll_sum - 1) % 10 + 1;
                r += freq * part2(cache, next_x, x_score + next_x, y, y_score, !turn);
            } else {
                let next_y = (y + roll_sum - 1) % 10 + 1;
                r += freq * part2(cache, x, x_score, next_y, y_score + next_y, !turn);
            }
        }
        r
    };
    cache.insert((x, x_score, y, y_score, turn), r);
    r
}

pub fn solve(input: String) -> (i32, i64) {
    let (x, y) = input
        .lines()
        .map(|x| x.split_once(": ").unwrap().1.parse().unwrap())
        .collect_tuple()
        .unwrap();

    let mut cache = Cache::new();
    let p1 = part2(&mut cache, x, 0, y, 0, true);
    let p2 = part2(&mut cache, y, 0, x, 0, false);
    (part1(x, y), p1.max(p2))
}
