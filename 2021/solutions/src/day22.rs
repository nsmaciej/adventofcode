use itertools::Itertools;

type Range = (i32, i32);

fn parse_range(text: &str) -> Range {
    let (start, end) = text.split_once("=").unwrap().1.split_once("..").unwrap();
    (start.parse().unwrap(), end.parse().unwrap())
}

fn count(steps: &[(bool, Range, Range, Range)]) -> i64 {
    let mut xs = Vec::new();
    let mut ys = Vec::new();
    let mut zs = Vec::new();
    for (_, x, y, z) in steps {
        xs.push(x.0);
        xs.push(x.1 + 1);
        ys.push(y.0);
        ys.push(y.1 + 1);
        zs.push(z.0);
        zs.push(z.1 + 1);
    }
    xs.sort();
    ys.sort();
    zs.sort();
    xs.dedup();
    ys.dedup();
    zs.dedup();

    let mut sum = 0;
    for x in 0..xs.len() - 1 {
        let (x2, x3) = (xs[x], xs[x + 1]);
        for y in 0..ys.len() - 1 {
            let (y2, y3) = (ys[y], ys[y + 1]);
            for z in 0..zs.len() - 1 {
                let (z2, z3) = (zs[z], zs[z + 1]);
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
