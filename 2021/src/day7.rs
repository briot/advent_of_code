fn read_file() -> Vec<u16> {
    let contents = std::fs::read_to_string("input_day7")
        .expect("input_day7 not found");
    contents.split(",")
        .map(|s| s.trim().parse::<u16>().unwrap())
        .collect()
}

fn cost(numbers: &Vec<u16>, x: u16) -> i64 {
    numbers
        .iter()
        .map(|&num| (num as i64 - x as i64).abs())
        .sum()
}

pub fn part1() {
    let numbers = read_file();
    let max: u16 = *numbers.iter().max().unwrap();
    let mut min = cost(&numbers, 0);

    // We can stop as soon as we found a local minima
    for x in 1 ..= max {
        let s = cost(&numbers, x);
        if s < min {
            min = s;
        } else {
            println!("Day07 Part1: min={} at={}", min, x - 1);
            return;
        }
    }
}

fn one_move(x1 : u16, x2 : u16) -> i64 {
    let dist = (x1 as i64 - x2 as i64).abs();
    (dist + 1) * dist / 2
}

fn cost2(numbers: &Vec<u16>, x: u16) -> i64 {
    numbers
        .iter()
        .map(|&num| one_move(num, x))
        .sum()
}


pub fn part2() {
    let numbers = read_file();
    let max: u16 = *numbers.iter().max().unwrap();
    let mut min = cost2(&numbers, 0);

    // We can stop as soon as we found a local minima
    for x in 1 ..= max {
        let s = cost2(&numbers, x);
        if s < min {
            min = s;
        } else {
            println!("Day07 Part2: min={} at={}", min, x - 1);
            return;
        }
    }
}
