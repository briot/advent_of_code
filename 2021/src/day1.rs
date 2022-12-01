use std::fs::File;
use std::io::{BufRead};

pub fn part1() {
    let file = File::open("input_day1").expect("File not found");
    let mut prev: Option<i32> = None;
    let mut increases = 0;

    for line in std::io::BufReader::new(file).lines() {
        if let Ok(str_depth) = line {
            let d = str_depth.parse::<i32>().unwrap();
            if let Some(x) = prev {
               if x < d {
                   increases += 1;
               }
            }
            prev = Some(d);
        }
    }
    println!("Day01 Part1: total increases: {}", increases);
}

pub fn part2() {
    let contents = std::fs::read_to_string("input_day1")
        .expect("File not found");
    let values: Vec<u32> = contents.lines()
        .map(|s| {s.parse::<u32>().unwrap()})
        .collect();

    let mut prev: Option<u32> = None;
    let mut increases = 0;

    for idx in 0 .. values.len() - 1 {
        let maxidx = std::cmp::min(idx + 2, values.len() - 1);
        let s: u32 = (&values[idx ..= maxidx]).iter().sum();
        if let Some(p) = prev {
            if p < s {
                increases += 1;
            }
        }
        prev = Some(s);
    }

    println!("Day01 Part1: total increases: {}", increases);
}
