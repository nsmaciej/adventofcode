use aoc::*;

fn main() -> Main {
    let mut pos = 0;
    let mut aim = 0;
    let mut depth = 0;

    for line in lines(2)? {
        let (cmd, n) = line.split_ascii_whitespace().collect_tuple().unwrap();
        let n: i32 = n.parse()?;
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

    puts(pos * aim);
    puts(pos * depth);
    Ok(())
}
