use std::ops;
use std::fs;
use std::io::{self, BufRead};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct V2 {
    x: i32,
    y: i32,
}

impl V2 {
    fn new(x: i32, y: i32) -> Self {
        V2 { x, y }
    }
}

impl ops::Add<V2> for V2 {
    type Output = V2;
    fn add(self, other: V2) -> Self::Output {
        V2 { x: self.x + other.x, y: self.y + other.y }
    }
}

impl ops::AddAssign<V2> for V2 {
    fn add_assign(&mut self, other: V2) {
        self.x = self.x + other.x;
        self.y = self.y + other.y;
    }
}

impl ops::Mul<i32> for V2 {
    type Output = V2;
    fn mul(self, k: i32) -> Self::Output {
        V2 { x: self.x * k, y: self.y * k }
    }
}

enum Orientation {
    Rising,
    Falling,
    Horizontal,
    Vertical,
}

impl Orientation {
    fn to_v2(&self) -> V2 {
        match &self {
            Orientation::Rising => V2 { x: 1, y: 1 },
            Orientation::Falling => V2 { x: 1, y: -1 },
            Orientation::Horizontal => V2 { x: 1, y: 0 },
            Orientation::Vertical => V2 { x: 0, y: 1 },
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Line {
    start: V2,
    end: V2,
}

/// An iterator for the points of a line.
#[derive(Debug, Clone, Copy)]
struct LinePointsIterator {
    end: V2,
    current: V2,
    delta: V2,
    finished: bool,
}

impl Iterator for LinePointsIterator {
    type Item = V2;
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished { return None }
        let c = self.current;
        self.current += self.delta;
        if c == self.end { self.finished = true; }
        Some(c)
    }
}

/// A line represented as a pair of points, a start and an end.
/// Invariant: The points of the line are in lexicographic order.
impl Line {
    /// Parses a line from a string of the form x1,y1 -> x2,y2
    fn parse(input: &str) -> Line {
        let words: Vec<&str> = input.split(" ").collect();
        let coord1: Vec<&str> = words[0].split(",").collect();
        let coord2: Vec<&str> = words[2].split(",").collect();
        let p1 = V2 {
            x: coord1[0].parse().unwrap(),
            y: coord1[1].parse().unwrap(),
        };
        let p2 = V2 {
            x: coord2[0].parse().unwrap(),
            y: coord2[1].parse().unwrap(),
        };
        if p1.x < p2.x || p1.x == p2.x && p1.y < p2.y {
            Line {
                start: p1,
                end: p2,
            }
        } else {
            Line {
                start: p2,
                end: p1,
            }
        }
    }

    fn orientation(&self) -> Orientation {
        match () {
            _ if self.start.x == self.end.x => Orientation::Vertical,
            _ if self.start.y == self.end.y => Orientation::Horizontal,
            _ if self.end.y > self.start.y => Orientation::Rising,
            _ => Orientation::Falling,
        }
    }

    fn delta(&self) -> V2 {
        V2 {
            x: self.end.x - self.start.x,
            y: self.end.y - self.start.y,
        }
    }

    fn points(&self) -> LinePointsIterator {
        LinePointsIterator {
            current: self.start,
            end: self.end,
            delta: self.orientation().to_v2(),
            finished: false,
        }
    }

    fn render_to(&self, g: &mut Grid) {
        for p in self.points() {
            let k = g.entry(p).or_insert(0);
            *k += 1;
        }
    }
}

type Grid = HashMap<V2, i32>;

fn main() {
    let mut grid = HashMap::new();
    let _ = io::BufReader::new(fs::File::open("input/day5.txt").unwrap())
        .lines()
        .map(|x| String::from(x.unwrap()))
        .map(|x| Line::parse(&x))
        // // commenting out the following filter checks all lines
        // // this is how to get part 2
        // .filter(|line| match line.orientation() {
        //     Orientation::Horizontal | Orientation::Vertical => true,
        //     _ => false,
        // })
        .fold(&mut grid, |mut grid, line| {
            line.render_to(&mut grid);
            grid
        });

    let count: i32 = grid.iter().map(|(_, v)| if *v > 1 { 1 } else { 0 }).sum();
    println!("{}", count)
}
