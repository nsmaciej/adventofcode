//! Arithmetic Logic Unit

use ahash::AHashMap;

// We find that the input consists of 14 (one per digit) 'chunks' of
// instructions, each differing only by 3 constants a, b, and c. This is clear
// using a program like:
//
//     xs = open("inputs/day24.txt").read().splitlines()
//     for y in range(0, 18):
//         print(" ".join(f"{xs[18 * c + y]:<10}" for c in range(14)))
//
// By inspecting each chunk we see they represent at the following equations:
//
//     x = w != z % 26 + b
//     z /= a
//     z *= 1 + x * 25
//     z += x * (w + c)
//
// We can futher simplify these into:
//
//     x = w != z % 26 + b
//     z = z/a + x(25 * z / a + c + w)
//
// Note a is always either 26 or 1. Also note when a = 1, b > 10 in the input.
// This means that for chunks where a = 1, it is never the case that x = 0,
// because our input digit w has to lie within [1, 9]. Hence z will always grow
// when a = 1.
//
// By insepcting the input we see the amount of a = 1 and a = 26 cases is always
// equal (7 each), so when a = 26 we must reduce z by setting x = 0.  This means
// when a = 26, our input w must always be z % 26 + b.

type Op = (bool, i64, i64); // a = 26, b, c.
type Cache = AHashMap<(i64, i64), Option<i64>>;

fn brute<const MIN: bool>(ops: &[Op], cache: &mut Cache, z: i64, k: i64) -> Option<i64> {
    let Some((&(a, b, c), rest)) = ops.split_first() else {
        return (z == 0).then_some(0);
    };

    if a {
        // a is 26. Use this chance to reduce z.
        let w = z % 26 + b;
        if matches!(w, 1..=9) {
            brute::<MIN>(rest, cache, z / 26, k / 10).map(|m| k * w + m)
        } else {
            None
        }
    } else if let Some(&r) = cache.get(&(k, z)) {
        // Use the cache. The case above is so cheap it's not worth caching.
        return r;
    } else {
        // a is 1. To get the largest number accept the largest 'working' digit and v.v.
        let mut r = None;
        let mut w = if MIN { 1 } else { 9 };
        loop {
            if let Some(m) = brute::<MIN>(rest, cache, 26 * z + c + w, k / 10) {
                r = Some(k * w + m);
                break;
            }
            if w == if MIN { 9 } else { 1 } {
                break;
            }
            w += if MIN { 1 } else { -1 };
        }
        cache.insert((k, z), r);
        r
    }
}

fn op_const(line: &str) -> i64 {
    line[6..].parse().unwrap() // Skip over "xxx r "
}

pub fn solve(input: String) -> (i64, i64) {
    let lines: Vec<&str> = input.lines().collect();
    let params: Vec<Op> = lines
        .chunks_exact(18)
        .map(|x| (op_const(x[4]) == 26, op_const(x[5]), op_const(x[15])))
        .collect();

    let k = 10_000_000_000_000;
    (
        brute::<false>(&params, &mut Cache::new(), 0, k).unwrap(),
        brute::<true>(&params, &mut Cache::new(), 0, k).unwrap(),
    )
}
