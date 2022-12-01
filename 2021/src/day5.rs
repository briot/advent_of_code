use std::fs::File;
use std::io::{BufRead};

const SIZE: usize = 1000;
type World = [[u32; SIZE]; SIZE];

struct Line {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
}

fn parse_line(s: &String) -> Line {
    let mut sp = s.split(" -> ");
    let origin = sp.next().unwrap();
    let target = sp.next().unwrap();
    let mut os = origin.split(",");
    let x1 = os.next().unwrap().parse().expect("x1 not found");
    let y1 = os.next().unwrap().parse().expect("y1 not found");
    let mut ts = target.split(",");
    let x2 = ts.next().unwrap().parse().expect("x2 not found");
    let y2 = ts.next().unwrap().parse().expect("y2 not found");
    Line { x1, y1, x2, y2 }
}

fn read_file() -> Vec<Line> {
    let file = File::open("input_day5").expect("input_day5 not found");
    std::io::BufReader::new(file)
        .lines()
        .map(|s| parse_line(&s.unwrap()))
        .collect()
}

fn draw_line(world: &mut World, line: Line, diagonal: bool) {
    let incx = (line.x2 - line.x1).signum();
    let incy = (line.y2 - line.y1).signum();

    if !diagonal && line.x1 != line.x2 && line.y1 != line.y2 {
        return;
    }

    let mut x = line.x1;
    let mut y = line.y1;
    loop {
        world[y as usize][x as usize] += 1;
        if x == line.x2 && y == line.y2 {
            break;
        }
        x += incx;
        y += incy;
    }
}

fn count_overlaps(world: &World) -> u32 {
    let mut count = 0;
    for row in world {
        for cell in row {
            if *cell > 1 {
                count += 1;
            }
        }
    }
    count
}

pub fn part1() {
    let mut world = [[0; SIZE]; SIZE];
    let lines = read_file();
    for line in lines {
        draw_line(&mut world, line, false);
    }
    println!("Day05 Part1: overlaps {}", count_overlaps(&world));
}

pub fn part2() {
    let mut world = [[0; SIZE]; SIZE];
    let lines = read_file();
    for line in lines {
        draw_line(&mut world, line, true);
    }
    println!("Day05 Part2: overlaps {}", count_overlaps(&world));
}
