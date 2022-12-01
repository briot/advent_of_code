use std::fs::File;
use std::io::{BufRead};

type Grid = Vec<Vec<i32>>;   //  negative values when seen

struct Game {
    numbers: Vec<u32>,
    boards: Vec<Grid>,
}

fn read_file() -> Game {
    let file = File::open("input_day4").expect("input_day4 not found");
    let lines: Vec<String> = std::io::BufReader::new(file)
        .lines()
        .map(|s| s.expect("could not parse line"))
        .collect();
    let mut boards: Vec<Grid> = Vec::new();

    for line_idx in 1 .. lines.len() {
        if let Some(line) = lines.get(line_idx) {
            if line.len() == 0 {
                boards.push(Vec::new());
                continue;
            }
            boards.last_mut().unwrap().push(line
                .split_whitespace()
                .map(|s| s.parse::<i32>().unwrap())
                .collect());
        }
    }

    Game {
        numbers: lines.get(0).unwrap()
            .split(",")
            .map(|s| s.parse::<u32>().unwrap())
            .collect(),
        boards: boards,
    }
}

fn play_on_board(val: u32, board: &mut Grid) {
    for row in board {
        for col in row {
            if *col == val as i32 {
                *col = -1;  // *col;
            }
        }
    }
}

fn is_won(board: &Grid) -> bool {
    // check along rows
    let mut num_cols = 0;

    for row in board {
        num_cols = row.len();
        if num_cols == row.into_iter().filter(|s| **s < 0).count() {
            return true;
        }
    }

    // check along cols
    for col in 0 .. num_cols {
        let mut negative = 0;
        for row in board {
            if row[col] < 0 {
                negative += 1;
            }
        }
        if negative == board.len() {
            return true;
        }
    }

    return false;
}

fn sum_of_unmarked(board: &Grid) -> u32 {
    let mut result: u32 = 0;
    for row in board {
        for col in row {
            if *col > 0 {
                result += *col as u32;
            }
        }
    }
    result
}

// pub fn print(board: &Grid) -> () {
//     for row in board {
//         println!("{:?}", row);
//     }
// }

pub fn part1() {
    let mut game = read_file();
    for n in &game.numbers {
        for g in &mut game.boards {
            play_on_board(*n, g);
            if is_won(&g) {
                let sum = sum_of_unmarked(&g);
                println!("Day04 Part1: played played={} sum={} score={}",
                         *n, sum, sum * *n);
                return;
            }
        }
    }
}

pub fn part2() {
    let mut game = read_file();
    for n in &game.numbers {
        for idx in (0 .. game.boards.len()).rev() {
            play_on_board(*n, &mut game.boards[idx]);
            if is_won(&game.boards[idx]) {
                if game.boards.len() == 1 {
                   let sum = sum_of_unmarked(&game.boards[0]);
                   println!("Day04 Part2: played played={} sum={} score={}",
                            *n, sum, sum * *n);
                   return;
                }
                game.boards.remove(idx);
            }
        }
    }
}
