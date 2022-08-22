use std::fs;
use std::io::{self, BufRead};

fn dist(x1: i32, x2: i32) -> i32 {
    let n = (x1 - x2).abs();
    n * (n+1) / 2
}

fn main() {
    let initial_positions: Vec<i32> = io::BufReader::new(fs::File::open("input/day7.txt").unwrap())
        .lines()
        .map(|line| line.unwrap())
        .next().unwrap()
        .split(",")
        .map(|x| x.parse().unwrap())
        .collect();

    // We have a list of starting positions Xs for all the crabs.
    // Want to find a point X' such that
    // Sum_i (X' - Xs[i])^2 is minimal
    // = (X' - Xs[0])^2 + ... + (X' - Xs[N-1])^2
    // = N * X'^2 - 2X'*(Sum_i Xs[i]) + Sum_i Xs[i]^2
    // Idea: find the zeroes of this parabola, then find the center.
    // This will give us the apex of the parabola, which minimizes its value.
    // Idea: Doesn't have to be the zeroes! Can be any fixed value for which we
    // can find two X values.
    // In particular, we can choose C, i.e. Sum_i Xs[i]^2, as the fixed value.
    // This has the benefit of simplifying the equation down to:
    // 0 = N*X'^2 - 2X'*(Sum_i Xs[i])
    //   = X' (N*X'^2 - 2*Sum_i Xs[i])
    // => X' = 0
    // OR X' = 2*Sum_i Xs[i] / N
    // Let K = Sum_i Xs[i]
    // then it works out to the average position again!?
    // Somehow, this doesn't actually give us the minimum.
    // I manually played with the bounds of target values to inspect and found
    // the minimum that way.

    println!("Total number of crabs {}", initial_positions.len());
    let sum = initial_positions.iter().sum::<i32>();
    println!("{} / {} = {}", sum, initial_positions.len(), sum / (initial_positions.len() as i32));
    let target = sum / (initial_positions.len() as i32);
    let costs: Vec<(i32, i32)> = ((400)..(500))
        .map(|target| (target, initial_positions.iter().map(|x| dist(*x, target)).sum()))
        .collect();

    // Very mysteriously, the calculation from above gives us the correct target
    // for the part two distance function, which "squared".

    for (target, cost) in costs {
        println!("Alignment cost for target {}: {}", target, cost);
    }
}
