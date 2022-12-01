use std::fs::File;
use std::io::BufRead;

fn read_file() -> Vec<Vec<u8>> {
    let file = File::open("input_day9").expect("input_day9 not found");
    std::io::BufReader::new(file)
        .lines()
        .map(|s| s.unwrap()
             .chars()
             .map(|c| c as u8 - '0' as u8)
             .collect())
        .collect()
}

//fn neighbors(grid: &Vec<Vec<u8>>, x: usize, y: usize) -> Vec<(usize, usize)> {
//    let mut result = Vec::new();
//    if x > 0 {
//        result.push((x - 1, y));
//    }
//    if y > 0 {
//        result.push((x, y - 1));
//    }
//    if x < grid.last().unwrap().len() - 1 {
//        result.push((x + 1, y));
//    }
//    if y < grid.len() - 1 {
//        result.push((x, y + 1));
//    }
//    result
//}

fn get(grid: &Vec<Vec<u8>>, x: usize, y: usize) -> u8 {
    match grid.get(y) {
        Some(r) => {
            match r.get(x) {
                Some(&c) => c,
                None     => u8::MAX,
            }
        },
        None    => u8::MAX,
    }
}

fn count_small_neighbors(
        grid: &Vec<Vec<u8>>,
        x: usize,
        y: usize) -> u8
{
//    let cell = grid[y][x];
//    let mut count = 0;
//    for (xn, yn) in neighbors(grid, x, y) {
//        if cell < grid[yn][xn]  {
//            count += 1;
//        }
//    }
//    count

    let cell = get(grid, x, y);
    let up = { if y > 0 { get(grid, x, y - 1) } else {u8::MAX} };
    let down = get(grid, x, y + 1);
    let left = { if x > 0 { get(grid, x - 1, y) } else {u8::MAX} };
    let right = get(grid, x + 1, y);
    (
        {if cell < up {1} else {0}} +
        {if cell < down {1} else {0}} +
        {if cell < right {1} else {0}} +
        {if cell < left {1} else {0}}
    )
}

fn find_low_points(grid: &Vec<Vec<u8>>) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize, usize)> = Vec::new();
    for y in 0 .. grid.len() {
        for x in 0 .. grid[y].len() {
            if count_small_neighbors(&grid, x, y) == 4 {
                result.push((x, y));
            }
        }
    }
    result
}

// Return the size of the basin that contains (x, y)
fn find_basin(
    grid: &Vec<Vec<u8>>,
    added: &mut Vec<Vec<bool>>,
    x: usize,
    y: usize) -> u64
{
    if added[y][x] || get(grid, x, y) == 9 {
        return 0;
    }
    added[y][x] = true;

    let mut size = 1;
    if x > 0 {
        size += find_basin(grid, added, x - 1, y);
    }
    if x < grid.first().unwrap().len() - 1 {
        size += find_basin(grid, added, x + 1, y);
    }
    if y > 0 {
        size += find_basin(grid, added, x, y - 1);
    }
    if y < grid.len() - 1 {
        size += find_basin(grid, added, x, y + 1);
    }
    size
}

pub fn part1() {
    let grid = read_file();
    let sum: u64 = find_low_points(&grid)
        .iter()
        .map(|(x, y)|  get(&grid, *x, *y) as u64 + 1)
        .sum();
    println!("Day08 Part1: sum={}", sum);
}

pub fn part2() {
    let grid = read_file();
    let mut basins: Vec<u64> = Vec::new();   // The size of each basin
    for (x, y) in find_low_points(&grid) {
        let mut added: Vec<Vec<bool>> = grid
            .iter()
            .map(|row|  row.iter().map(|_| false).collect())
            .collect();
        let size = find_basin(&grid, &mut added, x, y);
        basins.push(size);
    }
    basins.sort();
    basins.reverse();
    let size1 = basins[0];
    let size2 = basins[1];
    let size3 = basins[2];
    println!("Day08 Part2: basins={}", size1 * size2 * size3);
}
