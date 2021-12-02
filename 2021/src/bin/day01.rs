use aoc::*;

fn main() -> Main {
    let data: Vec<i32> = lines(1)?.iter().map(|x| x.parse()).try_collect()?;

    let answer = |window| {
        data.windows(window)
            .tuple_windows()
            .filter(|(x, y)| y.iter().sum::<i32>() > x.iter().sum())
            .count()
    };

    puts(answer(1));
    puts(answer(3));
    Ok(())
}
