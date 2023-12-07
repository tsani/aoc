{-# LANGUAGE NamedFieldPuns #-}

module Day5 where

import Control.Applicative
import Data.IntMap.Strict qualified as M
import Data.Maybe (fromJust)
import System.IO

import Parser

-- We basically have a bunch of functions described by ranges.
-- Seed -> Soil ;  Soil -> smth ; ... ; smth -> Location
-- By composing them all we get a function Seed -> Location
-- We're interested in finding the lowest location that corresponds to any of
-- some given seeds.
-- Just run all the seeds through the composite function and see what the
-- smallest location is duh. Not that complicated, no?
--
-- I guess the challenge, at least in part one is to construct these functions
-- from the text and then to compose them.
-- this is also the first time that splitting by lines becomes insufficient. We
-- actually have to parse the lines!

-- SYNTAX ---------------------------------------------------------------------
-- This is in close correspondence with the structure of the input file.
-- It describes an abstract syntax tree for the input.

type Id = Int
type Len = Int

type Name = String
data Range = Range { dstStart :: Id, srcStart :: Id, len :: Len }
  deriving Show
data RawMapping = RawMapping { rawSrc :: Name, rawDst :: Name, ranges :: [Range] }
  deriving Show
data Problem = Problem { startSeeds :: [Id], mappings :: [RawMapping] }
  deriving Show

-- PARSING --------------------------------------------------------------------
-- Turns a string into an AST.

parse :: String -> Problem
parse = snd . fromJust . runParser problem where
  problem :: Parser Problem
  problem = Problem <$> seeds <* newline <* newline <*> (mapping `sepBy` newline)

  seeds :: Parser [Int]
  seeds = string "seeds: " *> (number `sepBy` spaces)

  mapping :: Parser RawMapping
  mapping = RawMapping
    <$> letters <* string "-to-" -- source
    <*> letters <* spaces <* string "map:" <* newline -- destination
    <*> many range

  range :: Parser Range
  range = Range
    <$> number <* spaces
    <*> number <* spaces
    <*> number <* newline

-- SEMANTICS ------------------------------------------------------------------
-- This is what is _really_ described by the input.
-- RawMappings are actually functions Int -> Int
-- We're going to compile each RawMapping into a function

data Mapping = Mapping { src :: Name, dst :: Name, apply :: Id -> Id }

-- Actually this function will be realized by a balanced binary tree, which
-- gives an efficient way to find the value associated to the greatest key less
-- than a given one. So each each function will run in log n time where n is
-- around 20 or 30 tops. BST go brrrrrr

rangesToMap :: [Range] -> M.IntMap (Id, Len)
rangesToMap =
  M.fromList .
  map (\Range { dstStart, srcStart, len } -> (srcStart, (dstStart, len)))

compile :: RawMapping -> Mapping
compile RawMapping { rawSrc, rawDst, ranges } =
  Mapping { src = rawSrc, dst = rawDst, apply }
  where
    internalMap = rangesToMap ranges

    apply :: Id -> Id
    apply src = case M.lookupLE src internalMap of
      Just (src', (d, len))
        | src < src' + len -> d + (src - src')
        | otherwise -> src
      Nothing -> src

idMapping :: Name -> Mapping
idMapping name = Mapping { src = name, dst = name, apply = \x -> x }

composeMappings :: Mapping -> Mapping -> Mapping
composeMappings m1 m2 =
  Mapping { src = src m2, dst = dst m1, apply = apply m2 . apply m1 }

-- SOLUTION TO PART 1 ---------------------------------------------------------
-- Compile all mappings, compose, run all seeds through, see which location is
-- smallest.

answer1 :: Problem -> Id
answer1 Problem { startSeeds, mappings = rawMappings } = smallest where
  smallest =
    minimum .
    map snd .
    map (\seed -> (seed, seedToLocation `apply` seed)) $
    startSeeds

  seedToLocation =
    foldr composeMappings (idMapping "X") $
    map compile rawMappings

-- SOLUTION TO PART 2 ---------------------------------------------------------

-- So simply running the seeds through the composed function isn't going to cut
-- it anymore. Now we're dealing with ranges, so perhaps finding a way to run a
-- range through a mapping quickly will solve the problem. The issue is that
-- the input range might map into multiple different output ranges.

data Interval = I { istart :: !Int, ilen :: !Int }
  deriving Show

type InternalMapping = Interval -> [Interval]

compileIntervalMapping :: RawMapping -> IntervalMapping
compileIntervalMapping RawMapping { ranges } = f where
  internalMap = rangesToMap ranges

  -- | Considers the overlap of two intervals.
  -- `cut inner outer` sees whether `inner` fits inside of `outer`. If it
  -- doesn't it cuts it.
  -- Situation 1: it fits, so we output Nothing; no cut is necessary
  --    |----------inner----------|
  -- |---------------outer-------------|
  --
  -- Situation 2: it goes past the end, so we output 2 intervals obtained by
  -- cutting the inner interval in two at the boundary of the outer interval.
  --       |------------inner-------------------|
  -- |------------outer-------|
  --       |----output 1------|----output 2-----|
  cut :: Interval -> Interval -> Maybe (Interval, Interval)
  cut !inner !outer
    -- the inner interval fits entirely inside the outer
    | remaining >= ilen inner = Nothing
    | otherwise =
      Just
        ( I { istart = istart inner, ilen = remaining }
        , I { istart = istart outer, ilen = ilen inner - remaining }
        )
    where
      remaining = istart outer + ilen outer - istart inner

  f (I { istart, ilen })@i
    | istart >= ilen = []
    | otherwise = case M.lookupLE istart internalMap of
      Just (src, (dst, len))
        -- the Just tells us that src <= istart
        -- the src falls inside the range on the left
        | istart < src + len -> case cut i (I dst len) of
          Nothing ->

        | otherwise -> case M.lookupGT istart internalMap of
        -- why GT not GE? Well if it is equal, then we would have fallen into
        -- the LE case earlier, so we only need to check GT.
          Just (src, (dst, len)) ->
            -- the part mapped to identity is [istart,src)
            -- since src > istart, this interval is nonempty


debug Problem { startSeeds } = sum $ decomposeRanges startSeeds where
  decomposeRanges [] = []
  decomposeRanges (start:len:rest) = len : decomposeRanges rest

main :: IO ()
main = withFile "input/day5.txt" ReadMode $ \h ->
  print =<< debug . parse <$> hGetContents h

readInput = readFile "input/day5.txt"
