use std::collections::BTreeSet;

use itertools::Itertools;

type Range = (i32, i32);

fn parse_range(text: &str) -> Range {
    let (start, end) = text.split_once("=").unwrap().1.split_once("..").unwrap();
    (start.parse().unwrap(), end.parse().unwrap())
}

fn count(steps: &[(bool, Range, Range, Range)]) -> i64 {
    let mut xs = BTreeSet::new();
    let mut ys = BTreeSet::new();
    let mut zs = BTreeSet::new();
    for (_, x, y, z) in steps {
        xs.insert(x.0);
        xs.insert(x.1 + 1);
        ys.insert(y.0);
        ys.insert(y.1 + 1);
        zs.insert(z.0);
        zs.insert(z.1 + 1);
    }

    let mut sum = 0;
    for x2 in xs.iter().rev().skip(1).cloned() {
        let x3 = *xs.range(x2..).skip(1).next().unwrap();
        // println!("now on {x2}..{x3}");
        for y2 in ys.iter().rev().skip(1).cloned() {
            let y3 = *ys.range(y2..).skip(1).next().unwrap();
            for z2 in zs.iter().rev().skip(1).cloned() {
                let z3 = *zs.range(z2..).skip(1).next().unwrap();
                // See if the x1..x2, y1..y2, z1..z2 range is on or off.
                let mut on = false;
                for (state, (x1, x4), (y1, y4), (z1, z4)) in steps.iter().cloned() {
                    if (x2 >= x1 && x3 <= x4 + 1)
                        && (y2 >= y1 && y3 <= y4 + 1)
                        && (z2 >= z1 && z3 <= z4 + 1)
                    {
                        on = state;
                    }
                }
                if on {
                    sum += (x3 - x2) as i64 * (y3 - y2) as i64 * (z3 - z2) as i64;
                }
            }
        }
    }
    sum
}

pub fn solve(input: String) -> (i64, i64) {
    let steps = input
        .lines()
        .map(|line| {
            let (state, rest) = line.split_once(' ').unwrap();
            let (x, y, z) = rest.split(',').map(parse_range).collect_tuple().unwrap();
            (state == "on", x, y, z)
        })
        .collect_vec();

    let init_steps = steps
        .iter()
        .cloned()
        .filter(|(_, x, y, z)| {
            x.0 >= -50 && x.1 <= 50 && y.0 >= -50 && y.1 <= 50 && z.0 >= -50 && z.1 < 50
        })
        .collect_vec();

    (count(&init_steps), count(&steps))
}
