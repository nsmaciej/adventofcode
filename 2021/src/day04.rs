//! Giant Squid

#[derive(Debug)]
struct Field {
    marked: bool,
    value: i32,
}

#[derive(Debug)]
struct Bingo {
    board: Vec<Vec<Field>>,
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
        }
    }

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
        (0..5).any(|i| {
            let rows = (0..5).all(|j| self.board[i][j].marked);
            let cols = (0..5).all(|j| self.board[j][i].marked);
            rows || cols
        })
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
    let numbers: Vec<i32> = input[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards: Vec<Bingo> = input[2..].chunks(6).map(Bingo::parse).collect();

    let mut scores = Vec::new();
    for num in numbers {
        boards.retain_mut(|board| {
            board.mark(num);
            let won = board.won();
            if won {
                scores.push(board.unmarked_sum() * num);
            }
            !won
        })
    }

    (*scores.first().unwrap(), *scores.last().unwrap())
}
