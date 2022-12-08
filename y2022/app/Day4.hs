module Day4 where

import Data.Bifunctor
import Data.Tuple (swap)
import System.IO

type Range = (Int, Int)

-- | Splits a list on the first occurrence of the given element, which is dropped.
split :: Eq a => a -> [a] -> ([a], [a])
split x l = tail <$> break (x ==) l

parse :: String -> [(Range, Range)]
parse = map (both (both read . split '-') . split ',') . lines where
  both f = bimap f f

contains ((start1, stop1), (start2, stop2)) = start1 <= start2 && stop2 <= stop1

overlaps ((start1, stop1), (start2, stop2)) =
  start1 <= start2 && start2 <= stop1 || start1 <= stop2 && stop2 <= stop1

eitherOf f g x = f x || g x

answer1 :: String -> Int
answer1 = length . filter (eitherOf contains (contains . swap)) . parse

answer2 :: String -> Int
answer2 = length . filter (eitherOf overlaps (overlaps . swap)) . parse

main :: IO ()
main = withFile "input/day4.txt" ReadMode $ \h ->
  print =<< answer2 <$> hGetContents h
