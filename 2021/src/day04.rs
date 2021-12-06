//! Giant Squid

use crate::aoc::*;

#[derive(Debug)]
struct Field {
    marked: bool,
    value: i32,
}

#[derive(Debug)]
struct Bingo {
    board: Vec<Vec<Field>>,
    won: bool,
}

impl Bingo {
    fn parse(lines: &[String]) -> Bingo {
        fn parse_row(row: &str) -> Vec<Field> {
            row.split_whitespace()
                .map(|x| Field {
                    value: x.parse().unwrap(),
                    marked: false,
                })
                .collect()
        }

        Bingo {
            board: lines.iter().map(|x| parse_row(x)).collect(),
            won: false,
        }
    }

    fn mark(&mut self, num: i32) -> bool {
        for y in 0..5 {
            for x in 0..5 {
                if self.board[y][x].value == num {
                    self.board[y][x].marked = true;
                    // Clever idea from Jesse - only check the row/col we just marked.
                    let row = self.board[y].iter().all(|x| x.marked);
                    let col = (0..5).all(|i| self.board[i][x].marked);
                    return row || col;
                }
            }
        }
        false
    }

    fn unmarked_sum(&self) -> i32 {
        self.board
            .iter()
            .flatten()
            .filter(|x| !x.marked)
            .map(|x| x.value)
            .sum()
    }
}

pub fn solve(input: Vec<String>) -> (i32, i32) {
    let numbers: Vec<i32> = numbers(&input[0], ',').collect();
    let mut boards: Vec<Bingo> = input[2..].chunks(6).map(Bingo::parse).collect();

    let mut scores = Vec::with_capacity(numbers.len());
    for num in numbers {
        for board in &mut boards {
            if board.won {
                continue;
            }
            if board.mark(num) {
                scores.push(board.unmarked_sum() * num);
                board.won = true;
            }
        }
    }

    (*scores.first().unwrap(), *scores.last().unwrap())
}
