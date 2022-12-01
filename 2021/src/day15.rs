use std::fs::File;
use std::io::BufRead;
use std::process;

struct Map {
    grid: Vec<Vec<u8>>,
}

type Pos = (i32, i32);

impl Map {

    pub fn new(filename: &str) -> Result<Map, std::io::Error> {
        let file = File::open(filename)?;
        let grid = std::io::BufReader::new(file)
            .lines()
            .map(|s|
                 s.unwrap()
                 .chars()
                 .map(|c| c as u8 - '0' as u8)
                 .collect())
            .collect();
        Ok(Map { grid })
    }

    pub fn top_left(&self) -> Pos {
        (0, 0)
    }

    pub fn bottom_right(&self) -> Pos {
        (self.grid[0].len() as i32- 1, self.grid.len() as i32 - 1)
    }

    pub fn shortest_path(&self) -> Option<(Vec<Pos>, usize)> {
        let goal = self.bottom_right();
        pathfinding::directed::astar::astar(
            // start
            &self.top_left(),

            // neighbors
            |&(x, y)|
                vec![(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
                .into_iter()
                .map(|p: Pos|
                     match self.grid.get(p.1 as usize) {
                         Some(row) => match row.get(p.0 as usize) {
                             Some(cost) => (p, *cost as usize),
                             None       => (p, usize::MAX),
                        },
                        None  => (p, usize::MAX),
                    }
                )
                .filter(|(_, cost)|  *cost != usize::MAX),

            // heuristic
            |&(x, y)|
                ((x - goal.0).abs() + (y - goal.1).abs()) as usize,

            // success
            |&p|  p == goal,
        )
    }
}

pub fn part1() {
    let map = Map::new("input_day15").unwrap_or_else(|e| {
        println!("file not found input_day15: {}", e);
        process::exit(1);
    });
    if let Some((_, cost)) = map.shortest_path() {
        println!("Day15 Part1 cost={}", cost);
    }
}

fn nextval(c: u8) -> u8 {
    if c > 9 { c - 9 } else { c }
}


pub fn part2() {
    let map = Map::new("input_day15").unwrap_or_else(|e| {
        println!("file not found input_day15: {}", e);
        process::exit(1);
    });

    // The grid repeats 5 times horizontally and 5 times vertically,
    // while adjusting the cost by +1 every time.

    let mut grid: Vec<Vec<u8>> = Vec::new();
    for repeat_y in 0 .. 5 {
        for row in &map.grid {
            let mut newrow = Vec::new();
            for repeat_x in 0 .. 5 {
                let mut n: Vec<u8> = 
                    row.iter()
                    .map(|c| nextval(c + repeat_x + repeat_y))
                    .collect();
                newrow.append(&mut n);
            }
            grid.push(newrow);
        }
    }

    let map2 = Map { grid };

    if let Some((_, cost)) = map2.shortest_path() {
        println!("Day15 Part2 cost={}", cost);
    }
}
