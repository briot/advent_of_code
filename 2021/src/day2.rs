use regex::Regex;

pub fn part1() {
    let contents = std::fs::read_to_string("input_day2")
        .expect("File not found");
    let re = Regex::new(
        r#"(forward|up|down) (\d+)"#
    ).expect("Invalid regexp");

    let mut x = 0;
    let mut depth = 0;
    for line in contents.lines() {
        let groups = re.captures(line).unwrap();
        let val: u32 = groups.get(2).unwrap().as_str().parse().unwrap();
        match groups.get(1).unwrap().as_str() {
            "forward" => x += val,
            "up"      => depth -= val,
            "down"    => depth += val,
            _         => panic!("invalid line {}", line),
        }
    }
    println!("Day02 Part1: x={} depth={} result={}", x, depth, x * depth);
}

pub fn part2() {
    let contents = std::fs::read_to_string("input_day2")
        .expect("File not found");
    let re = Regex::new(r#"(forward|up|down) (\d+)"#).unwrap();
    let mut x = 0;
    let mut depth = 0;
    let mut aim = 0;
    for line in contents.lines() {
        let groups = re.captures(line).unwrap();
        let val: u64 = groups.get(2).unwrap().as_str().parse().unwrap();
        match groups.get(1).unwrap().as_str() {
            "forward" => {
                depth += aim * val;
                x += val;
            },
            "up"      => aim -= val,
            "down"    => aim += val,
            _         => panic!("invalid line {}", line),
        }
    }
    println!("Day02 Part2: x={} depth={} result={}", x, depth, x * depth);
}
