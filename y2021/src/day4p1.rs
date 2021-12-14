use std::fs;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug)]
struct BingoSquare {
    number: i32,
    checked: bool,
}

impl BingoSquare {
    fn new(number: i32, checked: bool) -> BingoSquare {
        BingoSquare { number, checked }
    }
}

#[derive(Debug)]
struct BingoCard(Vec<Vec<BingoSquare>>);

impl BingoCard {
    fn new(v: Vec<Vec<BingoSquare>>) -> BingoCard {
        BingoCard(v)
    }

    /// Checks off the given number on the bingo card, if it exists.
    fn check(&mut self, number: i32) {
        match find_entry_2d(&mut self.0, |x: &BingoSquare| x.number == number) {
            Some(x) => x.checked = true,
            None => {}
        }
    }

    fn wins(&self) -> bool {
        for row in self.0.iter() {
            if row.iter().fold(true, |acc, x| acc && x.checked) {
                return true;
            }
        }
        for i in 0..self.0[0].len() {
            if (0..self.0.len()).fold(true, |acc, j| acc && self.0[j][i].checked) {
                return true;
            }
        }
        return false;
    }
}

fn find_entry<'a, T, F>(v: &'a mut Vec<T>, predicate: F) -> Option<&'a mut T>
where
    F: Fn(&T) -> bool,
{
    for x in v {
        if predicate(&x) {
            return Some(x);
        }
    }
    None
}

fn find_entry_2d<'a, T, F>(v: &'a mut Vec<Vec<T>>, predicate: F) -> Option<&'a mut T>
where
    F: Fn(&T) -> bool,
{
    v.iter_mut()
        .map(|x| find_entry(x, &predicate))
        .filter(|x| x.is_some())
        .next()
        .flatten()
}

trait Matrix<T> {
    /// Obtains a mutable reference to the first element in the matrix
    /// satisfying a given predicate.
    fn find_entry_mut<'a, F: Fn(&T) -> bool>(&'a mut self, predicate: F) -> Option<&'a mut T>;
}

impl<T> Matrix<T> for Vec<Vec<T>> {
    fn find_entry_mut<'a, F>(&'a mut self, predicate: F) -> Option<&'a mut T>
    where
        F: Fn(&T) -> bool,
    {
        self.iter_mut()
            .map(|x| find_entry(x, &predicate))
            .filter(|x| x.is_some())
            .next()
            .flatten()
    }
}

#[derive(Debug)]
struct Puzzle {
    random_numbers: Vec<i32>,
    cards: Vec<BingoCard>,
}

fn next_or_die<T, I: Iterator<Item = T>>(it: &mut I) -> T {
    match it.next() {
        Some(x) => x,
        None => panic!("No next item!"),
    }
}

fn parse_boards<I: Iterator<Item = String>>(mut it: &mut I) -> Vec<BingoCard> {
    let mut boards = Vec::<BingoCard>::new();
    loop {
        boards.push(BingoCard::new(
            (0..5)
                .map(|_i| {
                    next_or_die(&mut it)
                        .split(" ")
                        .filter(|x| !x.is_empty())
                        .map(|x| BingoSquare::new(x.parse::<i32>().unwrap(), false))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>(),
        ));
        match it.next() {
            None => return boards,
            Some(x) if x.is_empty() => continue,
            _ => panic!("unexpected file contents"),
        }
    }
}

fn parse_puzzle(puzzleText: String) -> Puzzle {
    let mut it = puzzleText.split("\n").map(|x| String::from(x));
    let random_numbers: Vec<i32> = next_or_die(&mut it)
        .split(",")
        .map(|x| x.parse::<i32>().unwrap())
        .collect();
    let _skip = next_or_die(&mut it);
    let cards = parse_boards(&mut it);
    Puzzle {
        random_numbers,
        cards,
    }
}

fn main() {
    let puzzle = parse_puzzle(fs::read_to_string("input/day4.txt").unwrap());

    println!("{:?}", puzzle);
}
