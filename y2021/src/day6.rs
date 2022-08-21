use std::fs;
use std::io::{self, BufRead};

type FishTable = [usize; 9];

/// Makes one day elapse.
/// All fish with timer 0 generate a new fish with timer 8 _and_ themselves reset to timer 6.
/// All other fish shift over, decrementing their timers by one.
fn tick(t: &mut FishTable) {
    // record the count of fish that are going to spawn
    let spawners = t[0];
    // advance the timers of all the other fish
    for i in 1..9 {
        t[i-1] = t[i]
    }
    t[6] += spawners; // reset all spawned to timer 6
    t[8] = spawners; // create new fish with timer 8
}

fn main() {
    let initial_fish = io::BufReader::new(fs::File::open("input/day6.txt").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .next().unwrap()
        .split(",")
        .map(|x| x.parse().unwrap())
        .collect::<Vec<usize>>();

    let mut fish: FishTable = [0; 9];
    for k in initial_fish {
        fish[k] += 1;
    }

    // 80 for part 1, 256 for part 2
    let days = 256;

    for _ in 0..days {
        tick(&mut fish);
    }

    print!("Total fish after 80 days: {}", fish.iter().sum::<usize>());
}
