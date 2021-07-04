mod common;
use common::*;

fn main() {
    let lines: Vec<Vec<u8>> = aoc_input_lines(19)
        .into_iter()
        .map(|x| x.into_bytes())
        .collect();
    let start_x = lines[0]
        .iter()
        .position(|x| *x == b'|')
        .expect("could not find starting line");

    let mut seen_letters = String::new();
    let mut position = Point::new(start_x as i32, 0);
    let mut direction = Point::new(0, 1);
    let mut step = 0;
    loop {
        let c = lines[position];
        if c.is_ascii_alphabetic() {
            seen_letters.push(c as char);
        } else if c == b' ' {
            break;
        } else if c == b'+' {
            // Try going left or right.
            if lines[position + direction.turn_left()] != b' ' {
                direction = direction.turn_left();
            } else if lines[position + direction.turn_right()] != b' ' {
                direction = direction.turn_right();
            } else {
                panic!("found nowhere to turn");
            }
        }

        position += direction;
        step += 1;
    }
    println!("{}", seen_letters);
    println!("{}", step);
}
