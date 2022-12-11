{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Day10 where

import Data.Foldable

data Instr
  = Noop
  | Addx Int
type Program = [Instr]

-- | How long it takes to run an instruction.
instrTime = \case
  Noop -> 1
  Addx _ -> 2

parse :: String -> [Instr]
parse = map (parseLine . words) . lines where
  parseLine = \case
    ["noop"] -> Noop
    ["addx", n] -> Addx (read n)
    _ -> error "bad input"

data State = State { regX :: Int }

interpret :: State -> Instr -> State
interpret s = \case
  Noop -> s
  Addx n -> s { regX = n + regX s }

run :: State -> Program -> [State]
run _ [] = []
run s (i : pgm) = replicate (instrTime i) s ++ run (interpret s i) pgm

-- | Given an increasing list of indices, finds the elements from the given list
-- at those indices.
ixs :: [Int] -> [a] -> [a]
ixs [] _ = []
ixs _ [] = []
ixs (i : is) l = x : ixs (map (subtract (i + 1)) is) xs where
  (_, x : xs) = splitAt i l

initialState = State { regX = 1 }

answer1 :: String -> Int
answer1 =
  sum . zipWith (*) cycles . ixs (map (subtract 1) cycles) . map regX . run initialState . parse where
  cycles = [20,60,100,140,180,220]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks k l = x : chunks k xs where
  (x, xs) = splitAt k l

answer2 :: String -> [String]
answer2 = map (zipWith visible [0..]) . chunks 40 . run initialState . parse where
  visible i State { regX }
    | regX - 1 <= i && i <= regX + 1 = '#'
    | otherwise = '.'

printScreen :: [String] -> IO ()
printScreen = traverse_ putStrLn

main :: IO ()
main = printScreen =<< answer2 <$> readFile "input/day10.txt"
