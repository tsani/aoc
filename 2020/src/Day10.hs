{-# LANGUAGE ViewPatterns #-}

module Day10 where

import Data.List ( group, sort )
import Data.Set ( Set )
import qualified Data.Set as S

p1 :: IO ()
p1 = print . go . parse =<< readFile "input/day10.txt" where
  go = prod . map length . group . sort . diff . sort

  prod [x, y] = x * (y + 1)
  prod _ = error "uh oh"

pairs f l = zipWith f l (tail l)

diff ns = zipWith (-) (tail ns) ns

parse :: String -> [Int]
parse = (0:) . map read . lines

-- | Finds all the ways you can add together adjacent items in the list.
-- Satisfies:
-- sum l == sum l' for each l' in collapse l
-- length l - 1 == length l' for each l' in collapse l
collapse :: [Int] -> [[Int]]
collapse [] = []
collapse [x] = []
collapse (x:y:xs)
  | x + y <= 3 = (x + y:xs) : fmap (x:) (collapse (y:xs))
  | otherwise = fmap (x:) (collapse (y:xs))

decompositions :: [Int] -> Set [Int]
decompositions l = S.unions $ go (collapse l) where
  go [] = []
  go ls = S.fromList ls : go (ls >>= collapse)


p2 :: IO ()
p2 = print . product .
  map ((+1) . S.size . decompositions) .
  -- count the number of ways the run can be decomposed by adding
  -- together adjacent values, not exceeding 3
  filter (all (1 ==)) . -- take only runs consisting only of 1s
  group . -- collapse runs of the same value into their own list
  diff . -- make a sequence of successive differences
  sort .
  parse =<< readFile "input/day10.txt"
