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

compile :: RawMapping -> Mapping
compile RawMapping { rawSrc, rawDst, ranges } =
  Mapping { src = rawSrc, dst = rawDst, apply }
  where
    internalMap =
      M.fromList .
      map (\Range { dstStart, srcStart, len } -> (srcStart, (dstStart, len))) $
      ranges

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

-- SOLUTION -------------------------------------------------------------------
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

main :: IO ()
main = withFile "input/day5.txt" ReadMode $ \h ->
  print =<< answer1 . parse <$> hGetContents h

readInput = readFile "input/day5.txt"
