module Day1 (dayMain) where

import Parser

import Data.List (transpose, sort)
import Data.Maybe (fromJust)
import Data.Tuple

parseInput :: String -> ([(Int, Int)])
parseInput = fromJust . parseOnly go where
  go = many line

  line = (,) <$> int <* spaces <*> int <* lineBreak

-- part1 :: IO ()
-- part1 = _

both :: (a -> b) -> (a, a) -> (b, b)
both f = swap . fmap f . swap . fmap f

dayMain :: IO (Int, Int)
dayMain = do
  input <- parseInput <$> readFile "input/day1.txt"
  pure (part1 input, part2 (unzip input))
  where
    part1 = sum . map abs . uncurry (zipWith subtract) . both sort . unzip
    part2 (ls, rs) = sum $ map (\x -> x * occurrenceCount x rs) ls

    occurrenceCount x = sum . map (fromEnum . (== x))
