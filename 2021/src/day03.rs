use itertools::Itertools;

#[inline]
fn getbit(n: u32, bit: u32) -> bool {
    n & (1u32 << bit) > 0
}

fn common(data: &[u32], bit: u32) -> bool {
    let count = data.iter().filter(|x| getbit(**x, bit)).count();
    // Note it's important we return true if the there is no majority.
    count >= data.len() - count
}

fn iterate(n: u32, data: &[u32], invert: bool) -> u32 {
    let mut data = data.to_vec();
    for i in (0..n).rev() {
        let common = common(&data, i) ^ invert;
        data.retain(|x| getbit(*x, i) == common);
        if data.len() == 1 {
            return data[0];
        }
    }
    panic!();
}

pub fn solve(input: Vec<String>) -> (u32, u32) {
    let n = input[0].len() as u32; // We can't find this easily once we convert to u32.
    let data: Vec<u32> = input
        .into_iter()
        .map(|x| u32::from_str_radix(&x, 2).unwrap())
        .collect_vec();

    let mut gamma = 0;
    let mut epsilon = 0;
    for i in (0..n).rev() {
        gamma *= 2;
        epsilon *= 2;
        if common(&data, i) {
            gamma += 1;
        } else {
            epsilon += 1;
        }
    }

    let rating = iterate(n, &data, false) * iterate(n, &data, true);
    (gamma * epsilon, rating)
}
