{-# LANGUAGE ViewPatterns #-}

module Day2 where

-- | Checks if a number (a count of a letter) is in the required
-- bounds.
data Problem = Problem Int Int Char String

p1 :: IO ()
p1 = print =<< length . filter (run . parse) . lines <$> readFile "input/day2.txt"

p2 :: IO ()
p2 = print =<< length . filter (run' . parse) . lines <$> readFile "input/day2.txt"

parse :: String -> Problem
parse s = Problem (read minb) (read maxb) c password where
  (range, tail -> s') = span (' ' /=) s
  (minb, tail -> maxb) = span ('-' /=) range
  (init -> head -> c, tail -> password) = span (' ' /=) s'


run :: Problem -> Bool
run (Problem lo hi c s) = lo <= count && count <= hi where
  count = sum (map (\c' -> if c == c' then 1 else 0) s)


sub :: Int -> Int
sub x = x - 1

run' :: Problem -> Bool
run' (Problem (sub -> one) (sub -> two) c s) =
  1 == length (filter (\i -> s !! i == c) [one, two])
