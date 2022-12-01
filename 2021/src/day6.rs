
fn read_file() -> Vec<u8> {
    let contents = std::fs::read_to_string("input_day6")
        .expect("input_day6 not found");
    contents.split(",")
        .map(|s| s.trim().parse::<u8>().unwrap())
        .collect()
}

fn evolv(fishes: &mut Vec<u8>) {
    let len = fishes.len();
    for f in 0 .. len {
        if fishes[f] == 0 {
            fishes.push(8);  //  a new fish was born
            fishes[f] = 6;
        } else {
            fishes[f] -= 1;
        }
    }
}

pub fn part1() {
    let mut fishes = read_file();
    for _gen in 0 .. 80 {
        // println!("day {} {:?} len={}", gen, fishes, fishes.len());
        evolv(&mut fishes);
    }
    println!("Day06 Part1: fishes={}", fishes.len());
}

type Fishes = [u64; 9];  //  the population of fishes, by age
fn evolv2(fishes: &mut Fishes) {
    *fishes = [
        fishes[1],
        fishes[2],
        fishes[3],
        fishes[4],
        fishes[5],
        fishes[6],
        fishes[7] + fishes[0],
        fishes[8],
        fishes[0],  // newborns
    ];
}

pub fn part2() {
    let initial_fishes = read_file();
    let mut fishes = [0; 9];
    for f in initial_fishes {
        fishes[f as usize] += 1;
    }

    for _gen in 0 .. 256 {
        // println!("day {} {:?} len={}", _gen, fishes,
        //    fishes.iter().sum::<u64>());
        evolv2(&mut fishes);
    }

    println!("Day06 Part2: fishes={}", fishes.iter().sum::<u64>());
}
