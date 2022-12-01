use std::fs::File;
use std::io::BufRead;

fn read_file() -> Vec<String> {
    let file = File::open("input_day10").expect("input_day10 not found");
    std::io::BufReader::new(file)
        .lines()
        .map(|s| s.unwrap())
        .collect()
}

enum Status {
    Corrupted(char),
    Incomplete(String),
    Correct,
}

impl Status {

    fn from(line: &String) -> Status {
        let mut stack: Vec<char> = Vec::new();
        for c in line.chars() {
            match c {
                '(' => stack.push(')'),
                '[' => stack.push(']'),
                '{' => stack.push('}'),
                '<' => stack.push('>'),
                ')' | ']' | '}' | '>' => {
                    match stack.pop() {
                        Some(p) => {
                            if p!= c {
                                return Status::Corrupted(c);
                            }
                        },
                        None => {
                            return Status::Corrupted(c);
                        },
                    }
                },
                _  => {
                    println!("Invalid char on line {}", c);
                    return Status::Corrupted(c);
                },
            }
        }
        if stack.is_empty() {
            return Status::Correct;
        }
        Status::Incomplete(String::from_iter(stack))
    }

    fn syntax_score(&self) -> u64 {
        match self {
            Status::Corrupted(c) => {
                match c {
                    ')' => 3,
                    ']' => 57,
                    '}' => 1197,
                    '>' => 25137,
                    _   => panic!("Invalid character {} for score", c),
                }
            },
            _  => 0,
        }
    }

    fn complete_score(&self) -> u64 {
        match self {
            Status::Incomplete(s) => {
                let mut score = 0;
                for c in s.chars().rev() {
                    score *= 5;
                    match c {
                        ')' => score += 1,
                        ']' => score += 2,
                        '}' => score += 3,
                        '>' => score += 4,
                        _   => panic!("Unexpected char on stack {}", c),
                    }
                }
               score
            },
            _  => 0,
        }
    }
}

pub fn part1() {
    println!(
        "Day10 Part1: syntax={}",
        read_file()
        .iter()
        .map(|s| Status::from(s).syntax_score())
        .sum::<u64>());
}

pub fn part2() {
    let mut scores = read_file()
        .iter()
        .map(|line|     Status::from(line))
        .map(|status|   status.complete_score())
        .filter(|score| *score != 0)
        .collect::<Vec<u64>>();
    scores.sort();
    let middle = scores[scores.len() / 2];
    println!("Day10 Part2: complete={}", middle);

}
