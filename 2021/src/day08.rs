//! Seven Segment Search

fn deduce_digit(counts: &[u8; 7], pattern: &str) -> usize {
    // Sum of the frequencies of charcaters in each pattern is unique per digit.
    // Thanks Reddit.
    let sum = pattern.bytes().map(|x| counts[(x - b'a') as usize]).sum();
    match sum {
        42 => 0,
        17 => 1,
        34 => 2,
        39 => 3,
        30 => 4,
        37 => 5,
        41 => 6,
        25 => 7,
        49 => 8,
        45 => 9,
        _ => panic!("invalid pattern"),
    }
}

pub fn solve(input: String) -> (usize, usize) {
    let mut part1 = 0;
    let mut part2 = 0;

    for line in input.lines() {
        let (patterns, outputs) = line.split_once(" | ").unwrap();
        let mut counts = [0; 7];
        patterns
            .bytes()
            .filter(|x| *x != b' ')
            .for_each(|x| counts[(x - b'a') as usize] += 1);

        let out: Vec<usize> = outputs
            .split_ascii_whitespace()
            .map(|x| deduce_digit(&counts, x))
            .collect();

        part1 += out.iter().filter(|x| matches!(x, 1 | 4 | 7 | 8)).count();
        part2 += out[0] * 1000 + out[1] * 100 + out[2] * 10 + out[3];
    }

    (part1, part2)
}
