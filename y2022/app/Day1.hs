module Day1 (main, split) where

import System.IO
import Data.List (sortBy)

-- | Splits a list on every element satisfying a predicate. That element is dropped.
split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p l =
  let (x, rest) = break p l in
  x : if null rest then [] else split p (tail rest)

parse :: String -> [Int]
parse = map (sum . map read) . split null . lines

answer1 :: String -> Int
answer1 = maximum . parse

answer2 :: String -> Int
answer2 = sum . take 3 . sortBy (flip compare) . parse

main :: IO ()
main = withFile "input/day1.txt" ReadMode $ \h -> do
  print =<< answer2 <$> hGetContents h
