fn common(data: &[String], pos: usize) -> char {
    let mut count = 0;
    for x in data {
        if x.chars().nth(pos).unwrap() == '1' {
            count += 1;
        }
    }
    if count == data.len() / 2 && data.len() % 2 == 0 {
        return '1';
    }
    if count > data.len() / 2 {
        '1'
    } else {
        '0'
    }
}

pub fn solve(data: Vec<String>) -> (i32, i32) {
    let n = data[0].len();
    let mut gamma = 0;
    let mut epsilon = 0;
    for i in 0..n {
        gamma *= 2;
        epsilon *= 2;
        if common(&data, i) == '1' {
            gamma += 1;
        } else {
            epsilon += 1;
        }
    }

    let mut oxygen = data.clone();
    for i in 0..n {
        let c = common(&oxygen, i);
        oxygen.retain(|x| x.chars().nth(i).unwrap() == c);
        if oxygen.len() == 1 {
            break;
        }
    }

    let mut scrubber = data.clone();
    for i in 0..n {
        let c = common(&scrubber, i);
        scrubber.retain(|x| x.chars().nth(i).unwrap() != c);
        if scrubber.len() <= 1 {
            break;
        }
    }
    let r =
        i32::from_str_radix(&scrubber[0], 2).unwrap() * i32::from_str_radix(&oxygen[0], 2).unwrap();
    (gamma * epsilon, r)
}
