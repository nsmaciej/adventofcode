use aoc::*;

fn main() {
    let data: Vec<i32> = input_lines(1)
        .unwrap()
        .map(|x| x.unwrap().parse().unwrap())
        .collect();

    let answer = |window_size| {
        data.windows(window_size)
            .tuple_windows()
            .filter(|(x, y)| y.iter().sum::<i32>() > x.iter().sum())
            .count()
    };

    println!("{}", answer(1));
    println!("{}", answer(3));
}
