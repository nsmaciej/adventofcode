//! Syntax Scoring

fn part1_points(bracket: u8) -> i64 {
    match bracket {
        b')' => 3,
        b']' => 57,
        b'}' => 1197,
        b'>' => 25137,
        _ => panic!(),
    }
}

fn part2_points(bracket: u8) -> i64 {
    match bracket {
        b'(' => 1,
        b'[' => 2,
        b'{' => 3,
        b'<' => 4,
        _ => panic!(),
    }
}

pub fn solve(input: Vec<Vec<u8>>) -> (i64, i64) {
    let mut part1_score = 0;
    let mut part2_scores = Vec::with_capacity(input.len());

    'next_line: for line in input {
        let mut stack = Vec::with_capacity(line.len());
        for x in line {
            if matches!(x, b'[' | b'(' | b'<' | b'{') {
                stack.push(x);
            } else {
                let last = match stack.pop().expect("too many closing parens") {
                    b'(' => b')',
                    b'[' => b']',
                    b'{' => b'}',
                    b'<' => b'>',
                    _ => panic!("unexpected bracket"),
                };
                if x != last {
                    part1_score += part1_points(x);
                    continue 'next_line;
                }
            }
        }
        // Incomplete line.
        let mut score = 0;
        for x in stack.iter().rev() {
            score = score * 5 + part2_points(*x);
        }
        part2_scores.push(score);
    }

    let midpoint = part2_scores.len() / 2;
    (part1_score, *part2_scores.select_nth_unstable(midpoint).1)
}
