{-# LANGUAGE NamedFieldPuns, TupleSections, BangPatterns, RankNTypes, DataKinds, ScopedTypeVariables #-}

module Day12 where

import Grid as G

import Data.Bifunctor
import Data.List ( find )
import Data.Maybe ( catMaybes, fromJust )
import qualified Data.Vector as V
import GHC.TypeLits

type Height = Int

type Distance = Int

min4 :: Dir Int -> Int
min4 Dir { left = !left, right = !right, up = !up, down = !down } =
  min left $! min right $! min down $! up

data Input h w = Input { grid :: Grid h w Height, start :: Loc h w, stop :: Loc h w}
  deriving Show

parse :: String -> (forall h w. (KnownNat h, KnownNat w) => Input h w -> r) -> r
parse input k = sizedGrid result $ \grid -> k $ Input { grid, start, stop } where
  charGrid = lines input
  result = V.fromList $ map (V.fromList . map char2height) charGrid

  indexOf2D :: (a -> Bool) -> [[a]] -> Maybe (Int, Int)
  indexOf2D p grid =
    fmap (\(x, (y, _)) -> (x, y)) .
    find (p . snd . snd) .
    catMaybes .
    map sequenceA $
    enumerate (map (find (p . snd) . enumerate) grid)

  -- fin' is justified because indexOf2D is guaranteed to produce in-bounds indices.
  Just start = bimap fin' fin' <$> indexOf2D ('S' ==) charGrid
  Just stop = bimap fin' fin' <$> indexOf2D ('E' ==) charGrid

  enumerate = zip [0..]

  char2height :: Char -> Height
  char2height 'S' = 0
  char2height 'E' = 25
  char2height c = fromEnum c - fromEnum 'a'

findMinimalDistance :: forall h w. KnownSize h w => Loc h w -> GridZ h w Height -> [Distance]
findMinimalDistance dst = map (fst . extract) . iterate step . setup where
  inf = 1000000
  setup = fmap (inf, )
  step :: GridZ h w (Distance, Height) -> GridZ h w (Distance, Height)
  step = extend k where
    k g = (if pos g == dst then 0 else f (dir g), snd . extract $ g) where
      (_, h) = extract g
      f :: Dir (Maybe (GridZ h w (Distance, Height))) -> Distance
      f = (1+) . min4 . fmap (maybe inf (j . extract)) where
        j :: (Distance, Height) -> Distance
        j (d, h')
          | h' <= h + 1 = d
          | otherwise = inf

answer1 :: String -> Int
answer1 input = parse input go where
  go :: KnownSize h w => Input h w -> Int
  go Input { grid, start, stop } =
    f . dropWhile (> 10000) . findMinimalDistance stop . gridz start $ grid where
      f (x:y:z:xs)
        | x == y && y == z = x
        | otherwise = f xs

main :: IO ()
main = print . answer1 =<< readFile "input/day12.txt"
