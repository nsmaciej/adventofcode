//! Sonar Sweep

pub fn solve(data: Vec<i32>) -> (usize, usize) {
    (
        data.array_windows().filter(|[a, b]| b > a).count(),
        data.array_windows().filter(|[a, _, _, d]| d > a).count(),
    )
}
