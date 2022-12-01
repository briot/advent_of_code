use std::fs::File;
use std::io::BufRead;
use std::fmt;

struct Digits {
    on: [bool; 7],
}

impl fmt::Display for Digits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s: String = String::new();
        for d in 0 .. 7 {
            if self.on[d] {
                s.push((d as u8 + 'a' as u8) as char);
            }
        }

        write!(f, "{}", s)
    }
}

fn count_lighted(d: &Digits) -> usize {
    d.on.iter().filter(|&&c| c).count()
}

fn is_simple(d: &Digits) -> bool {
    match count_lighted(d) {
        2 | 3 | 4 | 7 => true,
        5 | 6         => false,
        _             => panic!("Unexpected number of lighted"),
    }
}

fn diffs(d1: &Digits, d2: &Digits) -> u8 {
    let mut count = 0;
    for segment in 0 .. 7 {
        if d1.on[segment] != d2.on[segment] {
            count += 1;
        }
    }
    count
}

fn count_simple(digits: &Vec<Digits>) -> usize {
    digits.iter()
        .filter(|c| is_simple(c))
        .count()
}

fn parse_code(c: &str) -> Digits {
    let mut d = Digits { on: [false; 7] };
    for b in c.bytes() {
        let val = (b - 'a' as u8) as usize;
        d.on[val] = true;
    }
    d
}

fn parse_codes(c: &[&str]) -> Vec<Digits> {
    c.iter()
        .map(|c| parse_code(c))
        .collect()
}

struct Puzzle {
    input: Vec<Digits>,   // the 10 input
    output: Vec<Digits>,   // the 4 output
}


// impl fmt::Display for Puzzle {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "input={:?} output={:?}", self.input, self.output)
//     }
// }

fn parse_line(s: &String) -> Puzzle {
    let sp: Vec<&str> = s.split(" ").collect();
    Puzzle {
        input: parse_codes(&sp[0 .. 10]),
        output: parse_codes(&sp[11 .. ]),
    }
}

fn find_by_number_of_ligthed(p: &Puzzle, num: usize) -> Vec<&Digits> {
    p.input.iter().filter(|c| count_lighted(c) == num).collect()
}


fn solve(p: &Puzzle) -> usize {
    let one = find_by_number_of_ligthed(p, 2)[0];
    let seven = find_by_number_of_ligthed(p, 3)[0];
    let four = find_by_number_of_ligthed(p, 4)[0];
    let height = find_by_number_of_ligthed(p, 7)[0];
    let with_five = find_by_number_of_ligthed(p, 5);
    let with_six = find_by_number_of_ligthed(p, 6);

    let df01 = diffs(with_five[0], with_five[1]);
    let df02 = diffs(with_five[0], with_five[2]);
    let df12 = diffs(with_five[1], with_five[2]);

    // with five includes 2, 3, 5
    // diffs:  (2,3)=2   (2,5)=4   (3,5)=2
    // only three has 2 diffs with each of the others
    let (three, two_or_five) = {
        if df01 + df02 == 4  && df01 + df12 == 6  && df02 + df12 == 6 {
            (with_five[0], (with_five[1], with_five[2]))
        } else if df01 + df02 == 6 && df01 + df12 == 4 && df02 + df12 == 6 {
            (with_five[1], (with_five[0], with_five[2]))
        } else if df01 + df02 == 6 && df01 + df12 == 6 && df02 + df12 == 4 {
            (with_five[2], (with_five[0], with_five[1]))
        } else { panic!("three not found") }
    };

    // with six includes 0, 6, 9
    // Diffs: (2,0)=3  (2,6)=3  (2,9)=3
    //        (5,0)=3  (5,6)=1  (5,9)=1
    let ds20 = diffs(two_or_five.0, with_six[0]);
    let ds26 = diffs(two_or_five.0, with_six[1]);
    let ds29 = diffs(two_or_five.0, with_six[2]);
//    println!("{} <> {} {} {} {} {} {}",
//             two_or_five.0, with_six[0], ds20, with_six[1], ds26,
//             with_six[2], ds29);

    let (two, five) = {
        if ds20 + ds26 + ds29 == 5      { (two_or_five.1, two_or_five.0) }
        else if ds20 + ds26 + ds29 == 9 { (two_or_five.0, two_or_five.1) }
        else { panic!("two and five not found") }
    };

    let ds50 = diffs(five, with_six[0]);
    let ds56 = diffs(five, with_six[1]);
    let ds59 = diffs(five, with_six[2]);
//    println!("{} <> {} {} {} {} {} {}",
//             five, with_six[0], ds50, with_six[1], ds56, with_six[2], ds59);
    let zero = {
        if ds50 == 3 && ds56 == 1 && ds59 == 1      { with_six[0] }
        else if ds50 == 1 && ds56 == 3 && ds59 == 1 { with_six[1] }
        else if ds50 == 1 && ds56 == 1 && ds59 == 3 { with_six[2] }
        else { panic!("zero not found") }
    };

    let nine = {
        if diffs(one, with_six[0]) == 4 && diffs(zero, with_six[0]) != 0 {
            with_six[0]
        } else if diffs(one, with_six[1]) == 4
            && diffs(zero, with_six[1]) != 0 {
            with_six[1]
        } else {
            with_six[2]
        }
    };

    let mut result: usize = 0;
    for o in &p.output {
        if diffs(zero, o) == 0 {
            result = result * 10 + 0;
        } else if diffs(one, o) == 0 {
            result = result * 10 + 1;
        } else if diffs(two, o) == 0 {
            result = result * 10 + 2;
        } else if diffs(three, o) == 0 {
            result = result * 10 + 3;
        } else if diffs(four, o) == 0 {
            result = result * 10 + 4;
        } else if diffs(five, o) == 0 {
            result = result * 10 + 5;
        } else if diffs(seven, o) == 0 {
            result = result * 10 + 7;
        } else if diffs(height, o) == 0 {
            result = result * 10 + 8;
        } else if diffs(nine, o) == 0 {
            result = result * 10 + 9;
        } else {
            result = result * 10 + 6;
        }
    }
    result
}

fn read_file() -> Vec<Puzzle> {
    let file = File::open("input_day8").expect("input_day8 not found");
    std::io::BufReader::new(file)
        .lines()
        .map(|s| parse_line(&s.unwrap()))
        .collect()
}

pub fn part1() {
    let puzzles = read_file();
    println!(
        "Day08 Part1: total simple={}",
        puzzles.iter().map(|p|  count_simple(&p.output)).sum::<usize>());
}

pub fn part2() {
    let puzzles = read_file();
    let s = puzzles.iter().map(|p| solve(p)).sum::<usize>();
    println!("Day08 Part2: sum={}", s);
}
