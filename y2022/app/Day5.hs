{-# LANGUAGE NamedFieldPuns #-}

module Day5 where

import Data.List ( transpose, foldl' )
import Data.Maybe ( catMaybes )
import qualified Data.Vector as V
import System.IO
import Text.Read ( readMaybe )

type Crate = Char
type State = V.Vector [Crate] -- first item on the list is the top of the stack
type StackId = Int -- a stack ID is a _one_-based index into the state
type Count = Int
type Action = (Count, StackId, StackId)

data Problem =
  Problem
  { initialState :: !State
  , program :: ![Action]
  }

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x l = uncurry (:) . fmap (splitOn x . tail) $ break (x ==) l where
  tail [] = []
  tail (_ : xs) = xs

parse :: String -> Problem
parse = parseLines . fmap tail . break null . lines where
  parseLines (picture, actions) =
    Problem { initialState = parsePicture picture, program = map parseAction actions }

  parsePicture :: [String] -> State
  parsePicture = V.fromList . map (foldr ($) []). transpose . map parseLine where
    parseLine (_ : '1' : _) = []
    parseLine l = map parseChar . everyFour $ tail l where
      everyFour [] = []
      everyFour (x:xs) = x : everyFour (drop 3 xs)

    parseChar ' ' = id
    parseChar c = (c :)

  parseAction :: String -> Action
  parseAction = tupleUp . catMaybes . map readMaybe . splitOn ' ' where
    tupleUp [x, y, z] = (x, y, z)

  {- The picture has trailing spaces on each line.
     Base case, looking at line index 1 gives a number, not a letter.
     Step case, we're looking at each index given by 1+4k, k < 10
     Idea: parse each letter into a function (letter :) and compose them all together to construct the stack.
     If there's no letter, parse to identity function.
  [J]             [F] [M]
  [Z] [F]     [G] [Q] [F]
  [G] [P]     [H] [Z] [S] [Q]
  [V] [W] [Z] [P] [D] [G] [P]
  [T] [D] [S] [Z] [N] [W] [B] [N]
  [D] [M] [R] [J] [J] [P] [V] [P] [J]
  [B] [R] [C] [T] [C] [V] [C] [B] [P]
  [N] [S] [V] [R] [T] [N] [G] [Z] [W]
   1   2   3   4   5   6   7   8   9
  -}

-- | Extracts the top element of each crate.
tops :: State -> [Crate]
tops = V.toList . V.map head

type Interpretation = Action -> State -> State
type Bin a = a -> a -> a

interpret :: Bin [Crate] -> Interpretation
interpret push (count, srcId, dstId) = snd . uncurry pushes . pops where
  -- | Transforms a given element of a vector, computing something on the side
  -- and an updated vector.
  modifyAt :: Int -> (a -> (b, a)) -> V.Vector a -> (b, V.Vector a)
  modifyAt i f s = let (x, r) = f (s V.! i) in (x, s V.// [(i, r)])

  pops :: State -> ([Crate], State)
  pops = modifyAt (srcId - 1) (splitAt count)

  pushes :: [Crate] -> State -> ((), State)
  pushes els = modifyAt (dstId - 1) (\stk -> ((), push stk els))

pushEach stk els = foldl' (flip (:)) stk els
pushTogether stk els = els ++ stk

executeWith :: Interpretation -> Problem -> State
executeWith f Problem { initialState, program } = foldl' (flip f) initialState program

answer1 = tops . executeWith (interpret pushEach) . parse
answer2 = tops . executeWith (interpret pushTogether) . parse

main :: IO ()
main = withFile "input/day5.txt" ReadMode $ \h ->
  print =<< answer2 <$> hGetContents h
