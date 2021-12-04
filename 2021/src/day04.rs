use std::fmt::Debug;

use crate::aoc::*;

#[derive(Debug)]
struct Field {
    marked: bool,
    value: i32,
}

#[derive(Debug)]
struct Board {
    board: Vec<Vec<Field>>,
    won: bool,
}

fn parse_board(lines: &[String]) -> Board {
    let board = lines
        .iter()
        .map(|x| {
            x.split_ascii_whitespace()
                .map(|x| Field {
                    value: x.parse().unwrap(),
                    marked: false,
                })
                .collect_vec()
        })
        .collect();
    Board { board, won: false }
}

impl Board {
    fn mark(&mut self, num: i32) {
        for y in 0..5 {
            for x in 0..5 {
                if self.board[y][x].value == num {
                    self.board[y][x].marked = true;
                }
            }
        }
    }

    fn won(&self) -> bool {
        for y in 0..5 {
            if self.board[y].iter().all(|x| x.marked) {
                return true;
            }
        }

        for x in 0..5 {
            if (0..5).all(|y| self.board[y][x].marked) {
                return true;
            }
        }

        return false;
    }

    fn unmarked_sum(&self) -> i32 {
        self.board
            .iter()
            .map(|row| {
                row.iter()
                    .filter(|x| !x.marked)
                    .map(|x| x.value)
                    .sum::<i32>()
            })
            .sum()
    }
}

pub fn solve(input: Vec<String>) -> (i32, i32) {
    let numbers: Vec<i32> = input[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards = input[2..].chunks(6).map(parse_board).collect_vec();

    let mut scores = Vec::new();
    for num in numbers {
        for board in &mut boards {
            if board.won {
                continue;
            }
            board.mark(num);
            if board.won() {
                board.won = true;
                scores.push(board.unmarked_sum() * num);
            }
        }
    }
    (scores[0], *scores.last().unwrap())
}
