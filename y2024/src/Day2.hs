module Day2 where

import Data.Maybe

import Parser
import Z

type Level = Int

dayMain = do
  input <- parseInput <$> readFile "input/day2.txt"
  pure (part1 input, part2 input)
  where
    part1 = sum . map (fromEnum . isSafe)
    part2 = sum . map (fromEnum . isSafeWithAtMostOneBadLevel)


parseInput :: String -> [[Level]]
parseInput = unsafeParseOnly input where
  input = many line
  line = int `sepBy` spaces <* lineBreak

successiveDifferences :: [Int] -> [Int]
successiveDifferences xs = zipWith subtract xs (drop 1 xs)

isSafe :: [Level] -> Bool
isSafe ls = (gradually $ increasing ls) || (gradually $ decreasing ls) where
  gradually = all (\d -> 1 <= d && d <= 3)
  increasing = successiveDifferences
  decreasing = successiveDifferences . map negate

isSafeWithAtMostOneBadLevel :: [Level] -> Bool
isSafeWithAtMostOneBadLevel ls =
  isSafe ls || any isSafe (removingEach . fromJust $ fromList ls)

removingEach :: Z a -> [[a]]
removingEach = toList . flip extend dropCenter where
  dropCenter :: Z a -> [a]
  dropCenter (Z ls _ rs) = reverse ls ++ rs
