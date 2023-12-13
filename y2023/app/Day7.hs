{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day7 (main) where

import Control.Applicative
import Data.Char
import Data.List (foldl', sort, sortBy, group)
import Data.Maybe ( fromJust )
import Data.Ord
import System.IO

import Parser

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord)

rankLetters =
  [ ( '2', Two )
  , ( '3', Three )
  , ( '4', Four )
  , ( '5', Five )
  , ( '6', Six )
  , ( '7', Seven )
  , ( '8', Eight )
  , ( '9', Nine )
  , ( 'T', Ten )
  , ( 'J', Jack )
  , ( 'Q', Queen )
  , ( 'K', King )
  , ( 'A', Ace )
  ]

data Hand = Hand { hand :: [Rank], sortedHand :: [Char], bid :: Int }

data HandKind = High | OneK | TwoK | ThreeK | Full | FourK | FiveK
  deriving (Eq, Ord)

kind :: Hand -> HandKind
kind Hand { sortedHand } =
  case sortBy (comparing (Down . length)) $ group sortedHand of
    [[_,_,_,_,_]] -> FiveK
    [[_,_,_,_],_] -> FourK
    [[_,_,_],[_,_]] -> Full
    [[_,_,_],[_],[_]] -> ThreeK
    [[_,_],[_,_],_] -> TwoK
    [[_,_],[_],[_],[_]] -> OneK
    [[_],[_],[_],[_],[_]] -> High
    _ -> error "impossible"

parse :: String -> [Hand]
parse = map (snd . fromJust . runParser hand) . lines where
  hand = do
    hand <- some $ satisfy (not . (' ' ==))
    spaces
    bid <- number
    pure $ Hand
      { hand = map (fromJust . (`lookup` rankLetters)) hand
      , sortedHand = sort hand
      , bid
      }

  rank = foldr (<|>) empty $ map (\(c, x) -> satisfy (c ==) *> pure x) rankLetters

instance Ord Hand where
  compare h1 h2 = comparing kind h1 h2 <> comparing hand h1 h2

instance Eq Hand where
  h1 == h2 = EQ == compare h1 h2

answer1 = sum . map (\(rank, Hand { bid }) -> rank * bid) . zip [1..] . sort

main :: IO ()
main = withFile "input/day7.txt" ReadMode $ \h ->
  print =<< answer1 . parse <$> hGetContents h
