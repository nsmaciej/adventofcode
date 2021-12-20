use itertools::Itertools;
use std::collections::BTreeSet;

type Point = [i32; 3];

#[rustfmt::skip]
static ORIENTATIONS: [Point; 24] = [
    [1, 2, 3], [-2, 1, 3], [-1, -2, 3], [2, -1, 3],
    [2, 1, -3], [-1, 2, -3], [-2, -1, -3], [1, -2, -3],
    [3, 1, 2], [-1, 3, 2], [-3, -1, 2], [1, -3, 2],
    [1, 3, -2], [-3, 1, -2], [-1, -3, -2], [3, -1, -2],
    [2, 3, 1], [-3, 2, 1], [-2, -3, 1], [3, -2, 1],
    [3, 2, -1], [-2, 3, -1], [-3, -2, -1], [2, -3, -1],
];

fn ix(point: Point, i: i32) -> i32 {
    // 1-based index which copies the sign of the index.
    point[i.abs() as usize - 1] * i.signum()
}

fn orient(point: Point, t: u8) -> Point {
    let [ox, oy, oz] = ORIENTATIONS[t as usize];
    [ix(point, ox), ix(point, oy), ix(point, oz)]
}

fn sub([x1, y1, z1]: Point, [x2, y2, z2]: Point) -> Point {
    [x1 - x2, y1 - y2, z1 - z2]
}

fn dist([x1, y1, z1]: Point, [x2, y2, z2]: Point) -> u32 {
    x1.abs_diff(x2) + y1.abs_diff(y2) + z1.abs_diff(z2)
}

fn parse_point(line: &str) -> Point {
    let mut point = [0; 3];
    for (i, x) in line.split(',').enumerate() {
        point[i] = x.parse().unwrap();
    }
    point
}

fn parse_scanners(input: String) -> Vec<Vec<Point>> {
    input
        .split("\n\n")
        .map(|scanner| scanner.lines().skip(1).map(parse_point).collect())
        .collect()
}

fn find_overlap(located: &[Point], target: &[Point]) -> Option<(u8, Point)> {
    for located_point in located {
        let deltas: BTreeSet<Point> = located.iter().map(|x| sub(*x, *located_point)).collect();
        for target_reference in target.iter() {
            for dir in 0..24 {
                let common = target
                    .iter()
                    .map(|x| orient(sub(*x, *target_reference), dir))
                    .filter(|x| deltas.contains(x))
                    .count();
                if common >= 12 {
                    let diff = sub(orient(*target_reference, dir), *located_point);
                    return Some((dir, diff));
                }
            }
        }
    }
    None
}

pub fn solve(input: String) -> (usize, u32) {
    let mut scanners = parse_scanners(input);
    let mut beacon_positions: BTreeSet<Point> = scanners[0].iter().cloned().collect();
    let mut located_scanners = vec![scanners.swap_remove(0)];
    let mut scanner_positions = Vec::new();

    while !scanners.is_empty() {
        let located = located_scanners.pop().unwrap();
        located_scanners.extend(scanners.drain_filter(|target| {
            let Some((dir, diff)) = find_overlap(&located, target) else { return false };
            scanner_positions.push(diff);
            for x in target {
                *x = sub(orient(*x, dir), diff);
                beacon_positions.insert(*x);
            }
            true
        }));
    }

    let part2 = scanner_positions
        .iter()
        .tuple_combinations()
        .map(|(a, b)| dist(*a, *b))
        .max()
        .unwrap();
    (beacon_positions.len(), part2)
}
