//! Dive!

pub fn solve(input: String) -> (i32, i32) {
    let mut pos = 0;
    let mut aim = 0;
    let mut depth = 0;

    for line in input.lines() {
        let (cmd, n) = line.split_once(' ').unwrap();
        let n: i32 = n.parse().unwrap();
        match cmd {
            "forward" => {
                pos += n;
                depth += aim * n;
            }
            "up" => aim -= n,
            "down" => aim += n,
            _ => panic!(),
        }
    }

    (pos * aim, pos * depth)
}
