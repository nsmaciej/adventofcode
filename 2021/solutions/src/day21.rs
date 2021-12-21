use itertools::Itertools;

fn parse(line: &str) -> i32 {
    line.split_once(": ").unwrap().1.parse().unwrap()
}

pub fn solve(input: String) -> (i32, i32) {
    let (mut x, mut y) = input.lines().map(parse).collect_tuple().unwrap();
    let (mut score_x, mut score_y) = (0, 0);

    let mut rolls = 1;
    let mut turn = true;
    while score_x < 1000 && score_y < 1000 {
        let d = (rolls * 3 + 3) % 100;
        if turn {
            x = (x + d - 1) % 10 + 1;
            score_x += x;
        } else {
            y = (y + d - 1) % 10 + 1;
            score_y += y;
        }
        rolls += 3;
        turn = !turn;
    }

    (score_x.min(score_y) * (rolls - 1), 0)
}
