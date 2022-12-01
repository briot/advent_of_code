use std::fs::File;
use std::io::{BufRead};

const NUM_BITS: usize = 12;
type Frequencies = [[u32; 2]; NUM_BITS];
const NULL_FREQ: Frequencies = [[0, 0]; NUM_BITS];


fn read_file() -> Vec<String> {
    let file = File::open("input_day3").expect("input_day3 not found");
    std::io::BufReader::new(file)
        .lines()
        .map(|s| s.expect("could not parse line"))
        .collect()
}

fn compute_freqs(values: &Vec<String>) -> Frequencies {
    let mut result: Frequencies = NULL_FREQ;
    for v in values {
        for (bit, byte) in v.chars().enumerate() {
            let b = if byte == '0' { 0 } else { 1 };
            result[bit][b] += 1;
        }
    }
    result
}

fn to_decimal(binary: &str) -> u32 {
    let mut result = 0;
    for bit in binary.chars() {
        result *= 2;
        if bit == '1' {
            result += 1;
        }
    }
    result
}

pub fn part1() {
    let values = read_file();
    let freqs = compute_freqs(&values);
    let mut gamma = 0;
    let mut epsilon = 0;
    for bit in 0 .. freqs.len() {
        epsilon *= 2;
        gamma *= 2;
        if freqs[bit][0] > freqs[bit][1] {
            epsilon += 1;
        } else {
            gamma += 1;
        }
    }
    println!(
        "Day03 Part1: gamma={} epsilon={} power consumption={}",
        gamma, epsilon, gamma * epsilon);
}

pub fn part2() {
    let mut oxygen = read_file();
    let mut co2 = oxygen.clone();

    let mut bit = 0;
    while oxygen.len() > 1 {
        let freqs = compute_freqs(&oxygen);
        let f = if freqs[bit][0] > freqs[bit][1] { '0' } else { '1' };
        oxygen = oxygen
            .into_iter()
            .filter(|c| c.chars().nth(bit).unwrap() == f)
            .collect();
        bit += 1;
    }

    let mut bit = 0;
    while co2.len() > 1 {
        let freqs = compute_freqs(&co2);
        let f = if freqs[bit][0] > freqs[bit][1] { '0' } else { '1' };
        co2 = co2
            .into_iter()
            .filter(|c| c.chars().nth(bit).unwrap() != f)
            .collect();
        bit += 1;
    }
    println!("Day03 Part2: oxygen={} co2={} life_support={}",
             to_decimal(&oxygen[0]),
             to_decimal(&co2[0]),
             to_decimal(&oxygen[0]) * to_decimal(&co2[0]));
}
