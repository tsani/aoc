{-# LANGUAGE LambdaCase, NamedFieldPuns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Day9 where

type Row = [Int]
type Input = [Row]

parse :: String -> Input
parse = map (map read . words) . lines

extrapolateFwd :: Row -> Int
extrapolateFwd r
  | all (0 ==) r = 0
  | otherwise = last r + extrapolateFwd derivative
  where
    derivative = zipWith (-) (tail r) r

extrapolateBwd :: Row -> Int
extrapolateBwd r
  | all (0 ==) r = 0
  | otherwise = head r - extrapolateBwd derivative
  where
    derivative = zipWith (-) (tail r) r

answer1 :: Input -> Int
answer1 = sum . map extrapolateFwd

answer2 :: Input -> Int
answer2 = sum . map extrapolateBwd

main :: IO ()
main = print . answer2 . parse =<< readFile "input/day9.txt"
