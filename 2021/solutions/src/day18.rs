//! Snailfish

type Sfn = Vec<(u8, u8)>;

fn parse_sfn(line: &str) -> Sfn {
    let mut depth: u8 = 0;
    let mut r = Sfn::new();
    for x in line.bytes() {
        match x {
            b'[' => depth += 1,
            b']' => depth -= 1,
            b'0'..=b'9' => r.push(((x - b'0') as u8, depth)),
            b',' | b' ' => {}
            _ => panic!("unexpected character"),
        };
    }
    r
}

fn explode_sfn(sfn: &mut Sfn) -> bool {
    for i in 0..sfn.len() - 1 {
        let (a, depth) = sfn[i];
        if depth >= 5 {
            let (b, _) = sfn[i + 1];
            sfn.splice(i..i + 2, [(0, depth - 1)]);
            if let Some(x) = sfn.get_mut(i.overflowing_sub(1).0) {
                x.0 += a;
            }
            if let Some(x) = sfn.get_mut(i + 1) {
                x.0 += b;
            }
            return true;
        };
    }
    false
}

fn split_sfn(sfn: &mut Sfn) -> bool {
    for (i, (x, depth)) in sfn.iter().cloned().enumerate() {
        if x >= 10 {
            sfn.splice(i..i + 1, [(x / 2, depth + 1), ((x + 1) / 2, depth + 1)]);
            return true;
        }
    }
    false
}

fn add_sfn(mut dest: Sfn, rhs: &[(u8, u8)]) -> Sfn {
    dest.extend_from_slice(rhs);
    dest.iter_mut().for_each(|(_, d)| *d += 1);
    while explode_sfn(&mut dest) || split_sfn(&mut dest) {}
    dest
}

fn magnitude_sfn(sfn: Sfn) -> i32 {
    fn calculate(sfn: &[(u8, u8)], depth: u8, i: &mut usize) -> i32 {
        let a = if sfn[*i].1 == depth {
            *i += 1;
            sfn[*i - 1].0 as i32
        } else {
            calculate(sfn, depth + 1, i)
        };
        let b = if sfn[*i].1 == depth {
            *i += 1;
            sfn[*i - 1].0 as i32
        } else {
            calculate(sfn, depth + 1, i)
        };

        3 * a + 2 * b
    }
    calculate(&sfn, 1, &mut 0)
}

pub fn solve(input: String) -> (i32, i32) {
    let nums: Vec<Sfn> = input.lines().map(parse_sfn).collect();

    let part1 = magnitude_sfn(nums[1..].iter().fold(nums[0].clone(), |a, x| add_sfn(a, x)));
    let mut part2 = 0;
    // N.B. snailfish number addition is not commutative.
    for i in 0..nums.len() {
        for j in 0..nums.len() {
            if i == j {
                continue;
            };
            part2 = part2.max(magnitude_sfn(add_sfn(nums[i].clone(), &nums[j])));
        }
    }

    (part1, part2)
}
