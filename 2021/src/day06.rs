fn simulate(fish: &mut [usize; 9], n: usize) -> usize {
    for _ in 0..n {
        let born = fish[0];
        fish.rotate_left(1);
        fish[6] += born;
    }
    fish.iter().sum()
}

pub fn solve(input: Vec<String>) -> (usize, usize) {
    let mut fish = [0; 9];
    for counter in input[0].split(",") {
        fish[counter.parse::<usize>().unwrap()] += 1;
    }
    (simulate(&mut fish, 80), simulate(&mut fish, 256 - 80))
}
