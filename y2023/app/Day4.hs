{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day4 where

import Control.Arrow
import Control.Applicative
import Control.Monad (ap)
import Data.Foldable
import Data.Maybe (fromJust)
import Data.IntMap.Strict qualified as M
import Data.Set qualified as S
import System.IO

import Prelude hiding (lookup)

import Parser

-- MODEL ----------------------------------------------------------------------

type CardId = Int
type Count = Integer
data Row = Row { id_ :: CardId, winning :: [Int], have :: [Int] }
type Input = [Row]

-- SOLUTION TO PART 1 ---------------------------------------------------------

points :: Row -> Int
points row = case winnerCount row of
  0 -> 0
  n -> 2^(n-1)

winnerCount :: Row -> Int
winnerCount Row { winning, have } =
  length $ filter (`S.member` S.fromList winning) have

answer1 :: Input -> Int
answer1 = sum . map points

-- SOLUTION TO PART 2 -- ------------------------------------------------------

-- CardId -> count of that scratchcard
type S = M.IntMap Count

-- a strict state monad
newtype State s a = State { runState :: s -> (s, a) }
  deriving Functor

instance Monad (State s) where
  State f >>= k = State $ \(!s) -> let (!s', x) = f s in runState (k x) s'

instance Applicative (State s) where
  (<*>) = ap
  pure x = State $ \s -> (s, x)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

gets :: (s -> s') -> State s s'
gets f = State $ \s -> (s, f s)

-- high level state manipulation operations
type Solver = State S

lookup :: CardId -> Solver Count
lookup i = gets (M.! i)

adjustFollowing :: CardId -> Int -> (Count -> Count) -> Solver ()
adjustFollowing _ 0 _ = pure ()
adjustFollowing i k f = modify (M.adjust f i) *> adjustFollowing (i+1) (k-1) f

solve :: Count -> Input -> Solver Count
solve !acc [] = pure acc
solve !acc (r:rs) = do
  copyCount <- lookup (id_ r)
  adjustFollowing (id_ r + 1) (winnerCount r) (copyCount +)
  solve (copyCount + acc) rs

answer2 :: Input -> Count
answer2 input = snd $ runState (solve 0 input) (initial input) where

initial :: Input -> S
initial = M.fromList . map (\r -> (id_ r, 1))

-- PARSING --------------------------------------------------------------------

parse :: String -> Input
parse = map (snd . fromJust . runParser row) . lines where
  row :: Parser Row
  row = do
    word "Card"
    id_ <- number
    word ":"
    winning <- many (lexeme number)
    word "|"
    have <- many (lexeme number)
    pure $ Row { id_, winning, have }

  word = lexeme . string
  lexeme p = p <* ws
  ws = many (string " ") *> pure ()

-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = withFile "input/day4.txt" ReadMode $ \h ->
  print =<< (answer1 &&& answer2) . parse <$> hGetContents h
