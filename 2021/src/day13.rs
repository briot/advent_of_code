use std::fs::File;
use std::io::BufRead;
use std::process;
use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

struct World {
    points: HashSet<Point>,
}

impl World {

    pub fn new(filename: &str, stop_at_first_fold: bool
               ) -> Result<World, std::io::Error>
    {
        let file = File::open(filename)?;
        let mut folds: Vec<Point> = Vec::new();
        let mut raw_points: Vec<Point> = Vec::new();
        for line in std::io::BufReader::new(file).lines() {
            match line {
                Ok(li) => {
                    if li.len() == 0 {
                        ()
                    } else if li.starts_with("fold along x=") {
                        folds.push(Point {
                            x: li[13 .. ].parse().unwrap(),
                            y: usize::MAX,
                        });
                    } else if li.starts_with("fold along y=") {
                        folds.push(Point {
                            x: usize::MAX,
                            y: li[13 .. ].parse().unwrap(),
                        });
                    } else {
                        let mut s = li.split(',');
                        raw_points.push(Point {
                            x: s.next().unwrap().parse().unwrap(),
                            y: s.next().unwrap().parse().unwrap(),
                        });
                    }
                },
                Err(_) => (),
            }
        }

        Ok(World {
            points: raw_points.iter().map(|pt| {
                let mut x = pt.x;
                let mut y = pt.y;
                for f in &folds {
                    if f.y == usize::MAX {
                        if x > f.x {
                            x = 2 * f.x - x;
                        }
                    } else {
                        if y > f.y {
                            y = 2 * f.y - y;
                        }
                    }
                    if stop_at_first_fold {
                        break;
                    }
                }
                Point {x, y}
            }).collect(),
        })
    }
}

impl fmt::Display for World {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let maxx: usize = self.points.iter().map(|p| p.x).max().unwrap();
        let maxy: usize = self.points.iter().map(|p| p.y).max().unwrap();
        let mut grid = vec![vec![' '; maxx + 1]; maxy + 1];

        for pt in &self.points {
            grid[pt.y][pt.x] = '#';
        }

        let s = grid.iter()
            .map(|row| row.iter().collect::<String>() + "\n")
            .collect::<String>();

        write!(f, "{}", s)
    }

}

pub fn part1() {
    let world = World::new("input_day13", true).unwrap_or_else(|e| {
        println!("file not found input_day13: {}", e);
        process::exit(1);
    });
    println!("Day13 Part1 num points={}", world.points.len());
}

pub fn part2() {
    let world = World::new("input_day13", false).unwrap_or_else(|e| {
        println!("file not found input_day13: {}", e);
        process::exit(1);
    });
    println!("Day13 Part2:\n{}", world);
}
