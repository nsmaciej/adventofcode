use aoc::*;

fn main() {
    let data: Vec<i32> = input_lines(1)
        .unwrap()
        .map(|x| x.unwrap().parse().unwrap())
        .collect();

    let part1 = data.iter().tuple_windows().filter(|(x, y)| y > x).count();
    let part2 = data
        .windows(3)
        .tuple_windows()
        .filter(|(x, y)| y.iter().sum::<i32>() > x.iter().sum())
        .count();

    println!("{}", part1);
    println!("{}", part2);
}
