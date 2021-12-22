//! Dirac Dice

use ahash::AHashMap;
use itertools::Itertools;
use std::mem::swap;

fn part1(mut x: u64, mut y: u64) -> u64 {
    let mut x_score = 0;
    let mut y_score = 0;
    let mut rolls = 1;
    while y_score < 1000 {
        let d = (rolls * 3 + 3) % 100;
        x = (x + d - 1) % 10 + 1;
        x_score += x;
        rolls += 3;
        swap(&mut x, &mut y);
        swap(&mut x_score, &mut y_score);
    }
    x_score.min(y_score) * (rolls - 1)
}

// Sum of three rolls and how often it will happen per turn.
static OUTCOMES: [(u64, i64); 7] = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)];

type Cache = AHashMap<u64, (i64, i64)>;

fn part2(cache: &mut Cache, x: u64, x_score: u64, y: u64, y_score: u64, turn: bool) -> (i64, i64) {
    // Save some time hashing.
    let key = x << 24 | x_score << 16 | y << 8 | y_score << 1 | (turn as u64);
    if let Some(r) = cache.get(&key) {
        return *r;
    }

    let wins = if y_score >= 21 {
        (0, 1)
    } else if x_score >= 21 {
        (1, 0)
    } else {
        let mut wins = (0, 0);
        for (roll_sum, freq) in OUTCOMES {
            let future_wins = if turn {
                let next_x = (x + roll_sum - 1) % 10 + 1;
                part2(cache, next_x, next_x + x_score, y, y_score, !turn)
            } else {
                let next_y = (y + roll_sum - 1) % 10 + 1;
                part2(cache, x, x_score, next_y, next_y + y_score, !turn)
            };
            wins.0 += freq * future_wins.0;
            wins.1 += freq * future_wins.1;
        }
        wins
    };
    cache.insert(key, wins);
    wins
}

pub fn solve(input: String) -> (u64, i64) {
    let (x, y) = input
        .lines()
        .map(|x| x.split_once(": ").unwrap().1.parse().unwrap())
        .collect_tuple()
        .unwrap();

    let part1 = part1(x as u64, y as u64);
    let part2 = part2(&mut Cache::new(), x, 0, y, 0, true);
    (part1, part2.0.max(part2.1))
}
