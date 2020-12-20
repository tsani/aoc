{-# LANGUAGE PartialTypeSignatures #-}

module Day9 where

import Data.Bifunctor

main :: IO ()
main = print =<< findBad . parse <$> readFile "input/day9.txt" where
  parse = map read . lines

findBad = uncurry go' . first reverse . splitAt 25 where
  go' :: [Int] -> [Int] -> _
  go' _ [] = error "ran out of numbers"
  go' prev (n:ns)
    | length prev /= 25 = error "fuck no"
    | otherwise = case [ 1 | x <- prev, y <- prev, x /= y, x + y == n ] of
    _:_ -> go' (n : init prev) ns
    [] -> n

p2 :: IO ()
p2 = print =<< go . parse <$> readFile "input/day9.txt" where
  parse = map read . lines

  go ns = findSum (findBad ns) [] ns where

findSum target prev [] = error "fuck"
findSum target prev (n:ns) = case sum prev `compare` target of
  EQ | length prev > 1 -> minimum prev + maximum prev
  LT -> findSum target (n:prev) ns
  GT -> findSum target (init prev) (n:ns)
