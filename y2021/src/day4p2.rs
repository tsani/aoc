use std::fs;
use std::io::{self, BufRead};
use std::mem;

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

impl Default for BingoCard {
    fn default() -> Self {
        BingoCard(Vec::default())
    }
}

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

    fn score(&self) -> i32 {
        self.0.iter().fold(0, |sum, x| {
            sum + x
                .iter()
                .filter(|x| !x.checked)
                .map(|x| x.number)
                .sum::<i32>()
        })
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

impl Puzzle {
    fn winners(self) -> Winners {
        Winners {
            index: 0,
            puzzle: self,
        }
    }
}

trait IteratorExt: Iterator {
    fn next_or_die(&mut self) -> Self::Item {
        match self.next() {
            Some(x) => x,
            None => panic!("no other item!"),
        }
    }
}

impl<T: Iterator> IteratorExt for T {}

fn parse_cards<I: Iterator<Item = String>>(it: &mut I) -> Vec<BingoCard> {
    let mut cards = Vec::<BingoCard>::new();
    loop {
        let card = BingoCard::new(
            (0..5)
                .map(|_i| {
                    it.next_or_die()
                        .split(" ")
                        .filter(|x| !x.is_empty())
                        .map(|x| BingoSquare::new(x.parse::<i32>().unwrap(), false))
                        .collect::<Vec<_>>()
                })
                .collect(),
        );
        cards.push(card);
        match it.next() {
            None => return cards,
            Some(x) if x.is_empty() => continue,
            _ => panic!("unexpected file contents"),
        }
    }
}

fn parse_puzzle<I: Iterator<Item = String>>(mut it: I) -> Puzzle {
    let random_numbers: Vec<i32> = it
        .next_or_die()
        .split(",")
        .map(|x| x.parse::<i32>().unwrap())
        .collect();
    let _skip = it.next_or_die();
    let cards = parse_cards(&mut it);
    Puzzle {
        random_numbers,
        cards,
    }
}

fn first_winning_card(p: Puzzle) -> Option<(i32, BingoCard)> {
    let numbers = p.random_numbers;
    let mut cards = p.cards;
    for x in numbers.iter() {
        for c in cards.iter_mut() {
            c.check(*x);
            if c.wins() {
                return Some((*x, mem::take(c)));
            }
        }
    }
    None
}

struct Winners {
    index: usize,
    puzzle: Puzzle,
}

/// Emits the winning cards in the order they win together with the
/// number called that made them win.
impl Iterator for Winners {
    type Item = (i32, BingoCard);

    fn next(&mut self) -> Option<Self::Item> {
        if 0 == self.puzzle.cards.len() || 0 == self.puzzle.random_numbers.len() {
            return None;
        }
        loop {
            if self.index == self.puzzle.cards.len() {
                let _ = self.puzzle.random_numbers.remove(0);
                self.index = 0;
            }

            let n = self.puzzle.random_numbers[0];

            self.puzzle.cards[self.index].check(n);

            if self.puzzle.cards[self.index].wins() {
                return Some((n, self.puzzle.cards.remove(self.index)));
            }

            self.index += 1;
        }
    }
}

fn main() {
    let b = io::BufReader::new(fs::File::open("input/day4.txt").unwrap())
        .lines()
        .map(|x| String::from(x.unwrap()));
    let puzzle = parse_puzzle(b);

    if let Some((last_called_number, last_winning_card)) = puzzle.winners().last() {
        println!("{}", last_winning_card.score() * last_called_number);
    } else {
        panic!("fuck");
    }
}
