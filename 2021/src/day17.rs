fn parse_range(text: &str) -> (i32, i32) {
    let text = text.split_once("=").unwrap().1;
    let (start, end) = text.split_once("..").unwrap();
    (start.parse().unwrap(), end.parse::<i32>().unwrap())
}

fn simulate(x1: i32, x2: i32, y1: i32, y2: i32, mut dx: i32, mut dy: i32) -> Option<i32> {
    let mut x = 0;
    let mut y = 0;
    let mut peak = 0;
    while x <= x2 && y >= y1 {
        if dy == 0 {
            peak = y;
        }
        if x >= x1 && x <= x2 && y <= y2 && y >= y1 {
            return Some(peak);
        }
        x += dx;
        y += dy;
        dx = 0.max(dx - 1);
        dy -= 1;
    }
    None
}

pub fn solve(input: String) -> (i32, i32) {
    let data = input.split_once(": ").unwrap().1;
    let (xs, ys) = data.split_once(", ").unwrap();
    let ((x1, x2), (y1, y2)) = (parse_range(xs), parse_range(ys));

    let mut best = 0;
    let mut hit = 0;
    // Quadratic to solve which triangular number will reach the target.
    let min_x = (1 + (1. + 8. * x1 as f32).sqrt() as i32) / 2;
    for dx in min_x..=(1 + x2 / 2) {
        for dy in y1..=y1.abs() {
            if let Some(r) = simulate(x1, x2, y1, y2, dx, dy) {
                hit += 1;
                best = best.max(r);
            }
        }
    }
    // If we shoot directly at the platform we only get one chance.
    // This assumes the platform width is smaller than it's x1 - 1.
    let trival_y = 1 + y2 - y1;
    let trival_x = 1 + x2 - x1;
    (best, hit + trival_y * trival_x)
}
