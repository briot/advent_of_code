use std::fs::File;
use std::io::BufRead;
use std::process;
use std::collections::HashMap;
use std::fmt;

struct Polymer {
    current: String,
    rules: HashMap<String, char>,
}

impl Polymer {

    pub fn new(filename: &str) -> Result<Polymer, std::io::Error> {
        let file = File::open(filename)?;
        let mut lines = std::io::BufReader::new(file).lines();
        let current = lines.next().unwrap().unwrap();
        lines.next();  // skip blank line

        let mut rules: HashMap<String, char> = HashMap::new();
        for line in lines {
            match line {
                Ok(li) => {
                    rules.insert(
                        String::from(&li[0 .. 2]),
                        li.chars().nth(6).unwrap(),
                    );
                },
                Err(e) => return Err(e),
            }
        }

        Ok(Polymer { current, rules })
    }

    pub fn step(&mut self) {
        let mut value = String::new();
        for idx in 0 .. self.current.len() - 1 {
            let pair = &(self.current[idx .. idx + 2]);
            let ins = self.rules.get(pair);
            match ins {
                Some(p) => {
                    value.push_str(&self.current[idx .. idx + 1]);
                    value.push(*p);
                },
                None    => (),
            };
        }
        value.push_str(&self.current[self.current.len() - 1 ..]);
        self.current = value;
    }

    pub fn score(&self) -> u32{
        let mut count : HashMap<char, u32> = HashMap::new();
        for c in self.current.chars() {
            match count.get_mut(&c) {
                Some(v) => { *v += 1; },
                None    => { count.insert(c, 1); },
            }
        }
        let mut minmax: Vec<_> = count
            .iter().map(|(_, v)| *v).collect();
        minmax.sort();
        minmax.last().unwrap() - minmax.first().unwrap()
    }
}

impl fmt::Display for Polymer {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.current)
    }

}

pub fn part1() {
    let mut poly = Polymer::new("input_day14").unwrap_or_else(|e| {
        println!("file not found input_day14: {}", e);
        process::exit(1);
    });
    for _step in 0 .. 10 {
        poly.step();
    } 
    println!("Day14 Part1 {}", poly.score());
}

struct Polymer2 {
    pairs: HashMap<(char, char), u64>,
    rules: HashMap<(char, char), char>,
    last_char: char,
}

impl Polymer2 {

    pub fn new(filename: &str) -> Result<Polymer2, std::io::Error> {
        let file = File::open(filename)?;
        let mut lines = std::io::BufReader::new(file).lines();
        let first = lines.next().unwrap().unwrap();
        let mut pairs = HashMap::new();
        let mut last_char = ' ';

        for (c1, c2) in first.chars().zip(first.chars().skip(1)) {
            *pairs.entry((c1, c2)).or_insert(0) += 1;
            last_char = c2;
        }

        lines.next();  // skip blank line

        let mut rules = HashMap::new();
        for line in lines {
            match line {
                Ok(li) => {
                    rules.insert(
                        (
                            li.chars().nth(0).unwrap(),
                            li.chars().nth(1).unwrap(),
                        ),
                        li.chars().nth(6).unwrap(),
                    );
                },
                Err(e) => return Err(e),
            }
        }

        Ok(Polymer2 { pairs, rules, last_char })
    }

    pub fn step(&mut self) {
        let mut pairs = HashMap::new();
        for ((c1, c2), count) in self.pairs.iter() {
            let newc = self.rules.get(&(*c1, *c2)).unwrap();
            *pairs.entry((*c1, *newc)).or_insert(0) += *count;
            *pairs.entry((*newc, *c2)).or_insert(0) += *count;
        }
        self.pairs = pairs;
    }

    pub fn score(&self) -> u64{
        let mut counts : HashMap<char, u64> = HashMap::new();
        for ((c1, _), count) in self.pairs.iter() {
            *counts.entry(*c1).or_insert(0) += count;
        }
        *counts.entry(self.last_char).or_insert(0) += 1;

        let mut minmax: Vec<_> = counts.values().collect();
        minmax.sort();
        *minmax.last().unwrap() - *minmax.first().unwrap()
    }
}


pub fn part2() {
    let mut poly = Polymer2::new("input_day14").unwrap_or_else(|e| {
        println!("file not found input_day14: {}", e);
        process::exit(1);
    });
    for _step in 0 .. 40 {
        poly.step();
    } 
    println!("Day14 Part2 {}", poly.score());
}
