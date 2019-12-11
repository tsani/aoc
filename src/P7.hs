{-# LANGUAGE RecordWildCards #-}

module P7 where

import Data.List (foldl', maximumBy, permutations)
import Data.Maybe (maybeToList, fromJust, isJust)
import Data.Ord (comparing)

import P5

-- | Given an initial computer state, supplies an initial phase and
-- input to calculate the output.
call :: State -> Int -> Int -> Int
call s p x = r where
  (State{_output = [r]}, _) = unInterp trace s { _input = [p, x] }

chain :: State -> [Int] -> Int
chain s = foldl' (flip $ call s) 0

phases1 = permutations [0..4]

main1 = do
  s <- loadState "inputs/p7.txt"
  print $ maximum $ map (chain s) phases1

phases2 = permutations [5..9]

-- | Generates a list of programs with their inputs having the
-- integer.
prepare :: State -> [Int] -> [State]
prepare s = map (\i -> s { _input = [i] })

-- Given an input to amplifier A and a list of states of the
-- amplifiers in the chain, generate the list of all outputs generated
-- by E until it halts.
feedback :: Int -> [State] -> [Int]
feedback i ss = case i' of
  Nothing -> [] -- if the amplifiers halt then there's no answer
  -- otherwise we record the output E produced and keep generating feedback
  Just i' -> i' : feedback i' ss'
  where
    (i', ss') = go (Just i) ss where
      go i [] = (i, []) -- I wish I could express go as a nice fold
      go i (s:ss) = (final, s':ss') where
        (final, ss') = go i' ss
        (s', i') = call' s (maybeToList i)

try :: State -> [Int] -> Int
try s phases = last $ feedback 0 (prepare s phases)

main2 = do
  s <- loadState "inputs/p7.txt"
  print $ maximum $ map (try s) phases2
