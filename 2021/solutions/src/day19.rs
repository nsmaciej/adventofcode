use ahash::AHashSet;
use itertools::Itertools;

type Fingerprint = u32;
type Point = [i32; 3];
type Scanner = (Vec<Point>, AHashSet<Fingerprint>);

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

fn fingerprint(a: Point, b: Point) -> Fingerprint {
    let [x, y, z] = sub(a, b);
    let i = (x.abs() + y.abs() + z.abs()) as u32;
    let j = (x * x + y * y + z * z) as u32;
    i << 16 | j & 0xffff
}

fn find_fingerprints(scanner: &[Point]) -> AHashSet<Fingerprint> {
    scanner
        .iter()
        .tuple_combinations()
        .map(|(a, b)| fingerprint(*a, *b))
        .collect()
}

fn parse_point(line: &str) -> Point {
    let mut point = [0; 3];
    for (i, x) in line.split(',').enumerate() {
        point[i] = x.parse().unwrap();
    }
    point
}

fn find_overlap(located: &Scanner, target: &Scanner) -> Option<(u8, Point)> {
    // Fingerprint colisions happen, so don't check against 66.
    if located.1.intersection(&target.1).count() < 50 {
        return None;
    }
    for located_point in &located.0 {
        let deltas: AHashSet<Point> = located.0.iter().map(|x| sub(*x, *located_point)).collect();
        for target_reference in &target.0 {
            'next_dir: for dir in 0..24 {
                let mut common = 0;
                for (i, x) in target.0.iter().enumerate() {
                    if deltas.contains(&orient(sub(*x, *target_reference), dir)) {
                        common += 1;
                        if common >= 12 {
                            // Enough of a match in practice.
                            let diff = sub(orient(*target_reference, dir), *located_point);
                            return Some((dir, diff));
                        }
                    }
                    if target.0.len() - i < 12 - common {
                        // We would've found at least one match by now.
                        continue 'next_dir;
                    }
                }
            }
        }
    }
    None
}

pub fn solve(input: String) -> (usize, u32) {
    let mut scanners = input
        .split("\n\n")
        .map(|scanner| {
            let scanner = scanner.lines().skip(1).map(parse_point).collect_vec();
            let fingerprints = find_fingerprints(&scanner);
            (scanner, fingerprints)
        })
        .collect_vec();
    let mut beacon_positions: AHashSet<Point> = scanners[0].0.iter().cloned().collect();
    let mut located_scanners = vec![scanners.swap_remove(0)];
    let mut scanner_positions = Vec::new();

    while !scanners.is_empty() && !located_scanners.is_empty() {
        let located = located_scanners.pop().unwrap();
        located_scanners.extend(scanners.drain_filter(|target| {
            let Some((dir, diff)) = find_overlap(&located, target) else { return false };
            scanner_positions.push(diff);
            for x in &mut target.0 {
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
