//! Trench Map

use crate::utils::Grid;

type Image = Vec<Vec<bool>>;

fn index(image: &Image, y: usize, x: usize, out: bool) -> u32 {
    let mut ix = 0;
    for dy in -1..=1 {
        for dx in -1..=1 {
            let r = image.getyx((y as i32 + dy) as usize, (x as i32 + dx) as usize);
            ix = (ix << 1) | *r.unwrap_or(&out) as u32;
        }
    }
    ix
}

fn simulate(algo: &[bool], image: Image, k: usize) -> usize {
    let image = (0..k).fold(image, |image, t| {
        let mut next = vec![vec![false; image.width() + 2]; image.height() + 2];
        // If the first bit of the algo is set, the entire universe blinks off and on.
        let out = algo[0] && t % 2 == 1;
        for y in 0..next.height() {
            for x in 0..next.width() {
                let r = index(&image, y.overflowing_sub(1).0, x.overflowing_sub(1).0, out) as usize;
                next[y][x] = algo[r];
            }
        }
        next
    });
    image.iter().flatten().filter(|x| **x).count()
}

pub fn solve(input: String) -> (usize, usize) {
    let mut lines = input.lines();
    let algo: Vec<bool> = lines.next().unwrap().bytes().map(|x| x == b'#').collect();
    assert_eq!(algo.len(), 512);

    let image: Image = lines
        .skip(1)
        .map(|line| line.bytes().map(|x| x == b'#').collect())
        .collect();

    (
        simulate(&algo, image.clone(), 2),
        simulate(&algo, image, 50),
    )
}
