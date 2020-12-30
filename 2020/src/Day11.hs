{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Data.Bool ( bool )
import Data.Foldable ( traverse_ )
import qualified Data.List.NonEmpty as N
import Control.Monad ( (>=>), mapM_ )

import Comonad
import ZZ
import Util ( iterateMaybe1, fix )


data Tile = Floor | Empty | Occupied deriving (Eq, Show)

p1 :: IO ()
p1 = print . (go (`cobind` rule1)) . parse =<< readFile "input/day11.txt"

p2 :: IO ()
p2 = print . (go (`cobind` rule2)) . parse =<< readFile "input/day11.txt"

pairs :: [a] -> [(a, a)]
pairs l = zip l (tail l)

t2c = \case
  Floor -> '.'
  Empty -> 'L'
  Occupied -> '#'

printZZ :: (a -> Char) -> ZZ a -> IO ()
printZZ f = traverse_ (putStrLn . N.toList . fmap f) . toList

go step = (sum . map count . concat . N.toList . fmap N.toList . toList) . fix step where
  count Occupied = 1
  count _ = 0

rule1 :: ZZ Tile -> Tile
rule1 z = decide . sum . map (maybe 0 countOccupied) $ neighbours where
  decide :: Int -> Tile
  decide n = case extract z of
    Empty | n == 0 -> Occupied
    Occupied | n >= 4 -> Empty
    x -> x

  neighbours :: [Maybe Tile]
  neighbours = map (fmap extract . ($ z)) directions where

rule2 :: ZZ Tile -> Tile
rule2 z = decide . sum . map (bool 0 1 . hasOccupied) $ directions where
  decide :: Int -> Tile
  decide n = case extract z of
    Occupied | n >= 5 -> Empty
    Empty | n == 0 -> Occupied
    x -> x

  hasOccupied :: (ZZ Tile -> Maybe (ZZ Tile)) -> Bool
  hasOccupied d = case filter (/= Floor) . map extract $ iterateMaybe1 d z of
    [] -> False
    Occupied:_ -> True
    Empty:_ -> False
    _ -> error "impossible"

directions :: [ZZ Tile -> Maybe (ZZ Tile)]
directions = [left >=> up, up, right >=> up, left, right, left >=> down, down, right >=> down]

countOccupied :: Tile -> Int
countOccupied = \case
  Occupied -> 1
  _ -> 0

parse :: String -> ZZ Tile
parse =
  fromList . N.fromList . map (N.fromList . map parseChar) . lines
  where
    parseChar '.' = Floor
    parseChar 'L' = Empty
    parseChar _ = error "impossible input"
