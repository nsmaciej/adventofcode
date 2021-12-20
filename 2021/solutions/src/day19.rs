use std::collections::BTreeSet;

use itertools::Itertools;

type Point = [i32; 3];

static DIRECTIONS: [Point; 6] = [
    [1, 2, 3],
    [2, 1, -3],
    [3, 1, 2],
    [1, 3, -2],
    [2, 3, 1],
    [3, 2, -1],
];
static ROTATIONS: [[i32; 2]; 4] = [[1, 2], [-2, 1], [-1, -2], [2, -1]];

/// 1-based index which copies the sign of the index.
fn ix(point: Point, i: i32) -> i32 {
    point[i.abs() as usize - 1] * i.signum()
}

/// Apply one of the 24 orientations.
fn orient(pt: Point, t: u8) -> Point {
    let dirs = DIRECTIONS[t as usize / 4];
    let [rx, ry] = ROTATIONS[t as usize % 4];
    [ix(pt, ix(dirs, rx)), ix(pt, ix(dirs, ry)), ix(pt, dirs[2])]
}

fn parse_scanners(input: String) -> Vec<Vec<Point>> {
    let mut lines = input.lines();
    let mut scanners = Vec::new();
    while let Some(scanner_line) = lines.next() {
        assert!(scanner_line.starts_with("--- scanner"));
        let mut probes = Vec::new();
        while let Some(probe_line) = lines.next() {
            if probe_line.is_empty() {
                break;
            }
            let mut point = [0; 3];
            for (i, x) in probe_line.split(',').enumerate() {
                point[i] = x.parse().unwrap();
            }
            probes.push(point);
        }
        scanners.push(probes);
    }
    scanners
}

fn sub([x1, y1, z1]: Point, [x2, y2, z2]: Point) -> Point {
    [x1 - x2, y1 - y2, z1 - z2]
}

/// Shifts rhs in place if successful.
fn find_overlap(
    lhs: &[Point],
    rhs: &mut [Point],
    positions: &mut BTreeSet<Point>,
) -> Option<Point> {
    for lhs_ref in lhs {
        let lhs_deltas: BTreeSet<Point> = lhs.iter().map(|x| sub(*x, *lhs_ref)).collect();
        for rhs_ref in rhs.iter() {
            for rhs_dir in 0..24 {
                let common = rhs
                    .iter()
                    .map(|x| orient(sub(*x, *rhs_ref), rhs_dir))
                    .filter(|x| lhs_deltas.contains(x))
                    .count();
                if common >= 12 {
                    // rhs_ref is the same beacon as lhs_ref.
                    let diff = sub(orient(*rhs_ref, rhs_dir), *lhs_ref);
                    for x in rhs {
                        *x = sub(orient(*x, rhs_dir), diff);
                        positions.insert(*x);
                    }
                    return Some([-diff[0], -diff[1], -diff[2]]);
                }
            }
        }
    }
    None
}

pub fn solve(input: String) -> (usize, u32) {
    let mut scanners = parse_scanners(input);
    let mut beacon_positions = scanners[0].iter().cloned().collect();
    let mut located_scanners = vec![scanners.remove(0)];
    let mut scanner_positions = Vec::new();

    while !scanners.is_empty() {
        let lhs = located_scanners.pop().unwrap();
        located_scanners.extend(scanners.drain_filter(|rhs| {
            if let Some(pos) = find_overlap(&lhs, rhs, &mut beacon_positions) {
                scanner_positions.push(pos);
                return true;
            }
            false
        }));
    }

    let part2 = scanner_positions
        .iter()
        .tuple_combinations()
        .map(|([x1, y1, z1], [x2, y2, z2])| x2.abs_diff(*x1) + y2.abs_diff(*y1) + z2.abs_diff(*z1))
        .max()
        .unwrap();
    (beacon_positions.len(), part2)
}
