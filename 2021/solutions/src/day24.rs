use itertools::Itertools;

type Params = (bool, i64, i64);

static Z_CAP: i64 = 26;

fn get_const(line: &str) -> i64 {
    line[6..].parse().unwrap() // Skip over "xxx r "
}

fn compute_x(z: i64, (_, b, _): Params, w: i64) -> i64 {
    (z % 26 + b != w) as i64
}

fn compute_z(z: i64, (a, b, c): Params, w: i64) -> i64 {
    let x = (z % 26 + b != w) as i64;
    // It's always 26 or 1 and this makes for better asm.
    let z_by_a = if a { z / Z_CAP } else { z };
    z_by_a + x * (25 * z_by_a + c + w)
}

fn brute(
    params: Params,
    rest: &[Params],
    z: i64,
    mult: i64,
    reducer: fn(i64, i64) -> i64,
) -> Option<i64> {
    if rest.is_empty() {
        assert_eq!(mult, 1);
        // Last digit, find the largest that still results in z = 0.
        let x = (1..=9)
            .filter(|&w| compute_z(z, params, w) == 0)
            .fold1(reducer);
        return x;
    }
    let mut best = None;
    for w in 1..=9 {
        if params.0 && compute_x(z, params, w) == 1 {
            continue;
        }
        let new_z = compute_z(z, params, w);
        if let Some(m) = brute(rest[0], &rest[1..], new_z, mult / 10, reducer) {
            if let Some(b) = best {
                best = Some(reducer(b, mult * w + m));
            } else {
                best = Some(mult * w + m);
            }
        }
    }
    best
}

pub fn solve(input: String) -> (i64, i64) {
    let lines: Vec<&str> = input.lines().collect();
    let params: Vec<Params> = lines
        .chunks_exact(18)
        .map(|x| (get_const(x[4]) == Z_CAP, get_const(x[5]), get_const(x[15])))
        .collect();

    let part1 = brute(
        params[0],
        &params[1..],
        0,
        10i64.pow(params.len() as u32 - 1),
        |x, y| x.max(y),
    )
    .unwrap();
    let part2 = brute(
        params[0],
        &params[1..],
        0,
        10i64.pow(params.len() as u32 - 1),
        |x, y| x.min(y),
    )
    .unwrap();
    (part1, part2)
}
