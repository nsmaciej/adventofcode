fn parse_range(text: &str) -> (i32, i32) {
    let text = text.split_once("=").unwrap().1;
    let (start, end) = text.split_once("..").unwrap();
    (start.parse().unwrap(), end.parse::<i32>().unwrap())
}

fn simulate(x1: i32, x2: i32, y1: i32, y2: i32, mut dx: i32, mut dy: i32) -> bool {
    let mut x = 0;
    let mut y = 0;
    while x <= x2 && y >= y1 {
        if x >= x1 && y <= y2 {
            return true;
        }
        x += dx;
        y += dy;
        dx = 0.max(dx - 1);
        dy -= 1;
    }
    false
}

pub fn solve(input: String) -> (i32, i32) {
    let data = input.split_once(": ").unwrap().1;
    let (xs, ys) = data.split_once(", ").unwrap();
    let ((x1, x2), (y1, y2)) = (parse_range(xs), parse_range(ys));

    // Quadratic to solve which triangular number will reach the target.
    let min_x = (1 + (1. + 8. * x1 as f32).sqrt() as i32) / 2;
    let easy_hits = (min_x..=(1 + x2 / 2))
        .flat_map(|dx| (y1..-y1).filter(move |dy| simulate(x1, x2, y1, y2, dx, *dy)))
        .count() as i32;

    (
        // The x for this doesn't matter unless it's really high.
        (-y1 * (-y1 - 1)) / 2,
        // If we shoot directly at the platform we only get one chance.
        // This assumes the platform width is smaller than it's x1 - 1.
        easy_hits + (1 + y2 - y1) * (1 + x2 - x1),
    )
}
