use itertools::Itertools;
use smallvec::SmallVec;

type Range = (i32, i32);
type Step = (bool, Range, Range, Range);

fn parse_range(text: &str) -> Range {
    let (start, end) = text.split_once("=").unwrap().1.split_once("..").unwrap();
    (start.parse().unwrap(), end.parse::<i32>().unwrap() + 1)
}

fn clip((start, end): Range, (clip_start, clip_end): Range) -> Option<Range> {
    (end > clip_start && start < clip_end).then(|| (clip_start.max(start), clip_end.min(end)))
}

fn count_untouched((_, xs, ys, zs): Step, remaining_steps: &[Step]) -> i64 {
    let conflicts: SmallVec<[Step; 8]> = remaining_steps // Covers 99% cases.
        .iter()
        .cloned()
        .flat_map(|(s, xt, yt, zt)| Some((s, clip(xs, xt)?, clip(ys, yt)?, clip(zs, zt)?)))
        .collect();
    let original = (xs.1 - xs.0) as i64 * (ys.1 - ys.0) as i64 * (zs.1 - zs.0) as i64;
    let removed: i64 = (0..conflicts.len())
        .map(|i| count_untouched(conflicts[i], &conflicts[i + 1..]))
        .sum();
    original - removed
}

fn solve_steps(steps: Vec<Step>) -> i64 {
    (0..steps.len())
        .filter(|i| steps[*i].0) // Only on steps.
        .map(|i| count_untouched(steps[i], &steps[i + 1..]))
        .sum()
}

pub fn solve(input: String) -> (i64, i64) {
    let steps: Vec<Step> = input
        .lines()
        .map(|line| {
            let (state, rest) = line.split_once(' ').unwrap();
            let (x, y, z) = rest.split(',').map(parse_range).collect_tuple().unwrap();
            (state == "on", x, y, z)
        })
        .collect();

    let init = (-50, 51);
    let init_steps = steps
        .iter()
        .cloned()
        .flat_map(|(s, xs, ys, zs)| Some((s, clip(xs, init)?, clip(ys, init)?, clip(zs, init)?)))
        .collect_vec();

    (solve_steps(init_steps), solve_steps(steps))
}
