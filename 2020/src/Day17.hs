{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Day17 where

import Data.Bool ( bool )
import Data.Set ( Set )
import qualified Data.Set as S

import Util ( Range, inRange )

type V3 = (Int, Int, Int)
type V4 = (Int, Int, Int, Int)

p1 :: IO ()
p1 = print . go . S.fromList . map to3 . parse =<< readFile "input/day17.txt" where
  go = S.size . (!! 6) . iterate (step rule3)

p2 :: IO ()
p2 = print . go . S.fromList . map to4 . parse =<< readFile "input/day17.txt" where
  go = S.size . (!! 6) . iterate (step rule3)

left, right, up, down, front, back :: V3
left = (-1, 0, 0)
right = (1, 0, 0)
up = (0, 1, 0)
down = (0, -1, 0)
front = (0, 0, 1)
back = (0, 0, -1)

class Grid a where
  (<+>) :: a -> a -> a

  neighbourDirs :: [a]

  neighbours :: a -> [a]
  neighbours p = map (p <+>) neighbourDirs

instance Grid (Int, Int, Int) where
  (x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

  neighbourDirs =
    [ up
    , up <+> front
    , up <+> back
    , up <+> left
    , up <+> left <+> front
    , up <+> left <+> back
    , up <+> right
    , up <+> right <+> front
    , up <+> right <+> back
    , left
    , left <+> front
    , left <+> back
    , right
    , right <+> front
    , right <+> back
    , front
    , back
    , down
    , down <+> front
    , down <+> back
    , down <+> left
    , down <+> left <+> front
    , down <+> left <+> back
    , down <+> right
    , down <+> right <+> front
    , down <+> right <+> back
    ]

instance Grid (Int, Int, Int, Int) where
  (w1, x1, y1, z1) <+> (w2, x2, y2, z2) = (w1 + w2, x1 + x2, y1 + y2, z1 + z2)

  neighbourDirs = S.toList . S.delete (0, 0, 0, 0) . S.fromList $
    [(w, x, y, z) | w <- r, x <- r, y <- r, z <- r] where
    r = [-1, 0, 1]

type Space = Set V3

type Rule = Bool -> Int -> Bool

rule3 True = (`inRange` (2, 3))
rule3 False = (3 ==)

step :: (Ord a, Grid a) => Rule -> Set a -> Set a
step rule s = S.filter (\p -> rule (p `S.member` s) (liveNeighbourCount p)) toConsider where
  toConsider = S.unions (map (\p -> S.fromList (p : neighbours p)) (S.toList s))

  liveNeighbourCount = sum . map (bool 0 1 . (`S.member` s)) . neighbours

to3 :: (Int, Int) -> V3
to3 (x, y) = (x, y, 0)

to4 :: (Int, Int) -> V4
to4 (x, y) = (x, y, 0, 0)

parse :: String -> [(Int, Int)]
parse =
  map (\(x, (z, _)) -> (x, z)) .
  filter (('#' ==) . snd . snd)  .
  concat .
  reassociate .
  zip [0 :: Int ..] .
  map (zip [0 :: Int ..])
  . lines

reassociate :: [(a, [(b, c)])] -> [[(a, (b, c))]]
reassociate = map . uncurry $ \i -> map . uncurry $ \j -> (i,) . (j,)
