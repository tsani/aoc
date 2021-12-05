use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn vec_transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    if v.is_empty() {
        return Vec::new();
    }
    let len = v[0].len();

    (0..len)
        .map(|i| v.iter().map(|r| r[i].clone()).collect())
        .collect()
}

#[derive(Clone, PartialEq)]
enum Bit {
    I,
    O,
}

impl Bit {
    fn parse(s: char) -> Self {
        match s {
            '1' => Bit::I,
            '0' => Bit::O,
            _ => panic!("bad bit"),
        }
    }

    fn flip(&self) -> Self {
        match self {
            Bit::I => Bit::O,
            Bit::O => Bit::I,
        }
    }

    fn to_weight(&self) -> i32 {
        match self {
            Bit::I => 1,
            Bit::O => -1,
        }
    }
}

trait BitArray {
    /// Computes the most common bit in the array. In case of a tie, 1 is considered more common.
    fn most_common_bit(&self) -> Bit;

    // Computes the least common bit in the array. In case of a tie, 0 is considered least common.
    fn least_common_bit(&self) -> Bit;

    /// Converts the bitarray to a single number, assuming MSB first.
    fn collapse(&self) -> i32;

    /// Flips all bits in the array.
    fn flip(&self) -> Self;
}

impl BitArray for Vec<Bit> {
    fn most_common_bit(&self) -> Bit {
        if self.iter().map(|x| x.to_weight()).sum::<i32>() >= 0 {
            Bit::I
        } else {
            Bit::O
        }
    }

    fn least_common_bit(&self) -> Bit {
        self.most_common_bit().flip()
    }

    fn collapse(&self) -> i32 {
        self.iter()
            .rev()
            .zip(0..)
            .map(|(b, k)| if let Bit::I = b { 2i32.pow(k) } else { 0 })
            .sum()
    }

    fn flip(&self) -> Self {
        self.iter().map(|x| x.flip()).collect()
    }
}

trait Matrix {
    type Item;
    fn column(&self, i: usize) -> Vec<Self::Item>;
}

impl<T> Matrix for Vec<Vec<T>>
where
    T: Clone,
{
    type Item = T;
    fn column(&self, i: usize) -> Vec<Self::Item> {
        self.iter().map(|x| x[i].clone()).collect()
    }
}

fn get_rating(matrix: Vec<Vec<Bit>>, target_bit: fn(Vec<Bit>) -> Bit) -> Vec<Bit> {
    match &(0..matrix[0].len()).fold(matrix, |matrix, i| {
        if matrix.len() <= 1 {
            matrix
        } else {
            let b = target_bit(matrix.column(i));
            matrix.into_iter().filter(|row| row[i] == b).collect()
        }
    })[..]
    {
        [x] => x.clone(),
        _ => panic!("impossible"),
    }
}

fn main() {
    let file = File::open(Path::new("input/day3.txt")).unwrap();
    let matrix: Vec<Vec<Bit>> = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().chars().map(Bit::parse).collect())
        .collect();

    let oxygen_rating = get_rating(matrix.clone(), oxygen).collapse();
    let co2_rating = get_rating(matrix, co2).collapse();
    println!("{}", oxygen_rating * co2_rating);
}

fn oxygen(column: Vec<Bit>) -> Bit {
    column.most_common_bit()
}

fn co2(column: Vec<Bit>) -> Bit {
    column.least_common_bit()
}
