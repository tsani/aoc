{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day8 where

import Data.Bifunctor
import qualified Data.Vector as V

import Grid

type Score = Int
type Count = Int
type Height = Int

parse :: String -> (forall h w. KnownSize h w => Grid h w Height -> r) -> r
parse input = sizedGrid (V.fromList . map (V.fromList . map (read . pure)) . lines $ input)

-- | Equips each point in the grid with a 4-tuple indicating for each direction
-- how tall the tallest tree is.
findTallestTrees :: forall h w. KnownSize h w => GridZ h w Height -> GridZ h w (Dir Height, Height)
findTallestTrees = extend f where
  f :: GridZ h w Height -> (Dir Height, Height)
  f g = error "don't care" where
  -- g' = GridZ
  --   { here = (collapse (fmap here <$> dir g'), here g)
  --   , index = index g
  --   , dir = fmap findTallestTrees <$> dir g
  --   }
    collapse x = (\d -> maybe (-1) (uncurry max . first d)) <$> dirs <*> x

-- | Calculates for each tree T a 4-tuple indicating for each direction
-- how many trees that T can see before the edge or a tree of equal or greater height.
findViewingDistances :: forall h w. KnownSize h w => GridZ h w Height -> GridZ h w (Dir Count)
findViewingDistances = extend k where
  k :: GridZ h w Height -> Dir Count
  k g = length . takeWhile1 (< extract g) <$> lookEverywhere g where
    takeWhile1 p l = case span p l of
      (xs, []) -> xs -- when we can see all the way to the edge
      (xs, x : _) -> x : xs -- when we run into a tree, we need to count it!

scenicScore :: Dir Count -> Score
scenicScore = product

-- | A tree is visible if it is taller than the tallest tree in at least one
-- direction.
visibleTrees :: GridZ h w (Dir Height, Height) -> GridZ h w Bool
visibleTrees = fmap $ \(d, h) -> any id $ (h >) <$> d

answer1, answer2 :: String -> Int
answer1 input = parse input $
  sum . map (length . filter id) . materialize . visibleTrees . findTallestTrees . gridz0

answer2 input = parse input $
  maximum . fmap maximum . materialize . fmap scenicScore . findViewingDistances . gridz0

main :: IO ()
main = print =<< answer2 <$> readFile "input/day8.txt"
