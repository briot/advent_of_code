use std::fs::File;
use std::io::BufRead;
use std::cmp;

struct World {
    grid: Vec<Vec<u8>>,
    sizex: usize,
    sizey: usize,
}

fn read_file() -> World {
    let file = File::open("input_day11")
        .expect("input_day11 not found");
    let grid: Vec<Vec<u8>> = std::io::BufReader::new(file)
        .lines()
        .map(|s| s.unwrap()
             .chars()
             .map(|c| c as u8 - '0' as u8)
             .collect())
        .collect();
    World {
        sizey: grid.len(),
        sizex: grid[0].len(),
        grid,
    }
}

fn flash(world: &mut World, x: usize, y: usize) {
    if world.grid[y][x] != 0 {
        world.grid[y][x] += 1;
        if world.grid[y][x] > 9 {
            world.grid[y][x] = 0;
            for x1 in cmp::max(1, x) - 1 ..= x + 1 {
                for y1 in cmp::max(1, y) - 1 ..= y + 1 {
                    if x1 < world.sizex && y1 < world.sizey {
                        flash(world, x1, y1);
                    }
                }
            }
        }
    }
}


// Returns number of flashes
fn one_step(world: &mut World) -> u32 {
    for y in 0 .. world.sizey {
        for x in 0 .. world.sizex {
            world.grid[y][x] += 1;
        }
    }

    for y in 0 .. world.sizey {
        for x in 0 .. world.sizex {
            if world.grid[y][x] > 9 {
                flash(world, x, y);
            }
        }
    }

    let mut count = 0;
    for y in 0 .. world.sizey {
        for x in 0 .. world.sizex {
            if world.grid[y][x] == 0 {
                count += 1;
            }
        }
    }

    count
}

// fn display(world: &World) {
//     for y in 0 .. world.sizey {
//         println!("{}", world.grid[y]
//                  .iter()
//                  .map(|c| (c + '0' as u8) as char)
//                  .collect::<String>());
//     }
// }

pub fn part1() {
    let mut world = read_file();
    let mut total = 0;
    const MAX: usize = 100;
    for _ in 1 ..= MAX {
        let count = one_step(&mut world);
        total += count;
//        display(&world);
    }
    println!("Day11 Part1: step {} total {}", MAX, total);
}

pub fn part2() {
    let mut world = read_file();
    let mut step = 1;
    loop {
        let count = one_step(&mut world);
        if count as usize == world.sizex * world.sizey {
            println!("Day11 Part2: all lighted at step {}", step);
            break;
        }
        step += 1;
    }
}
