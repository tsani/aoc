{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, NamedFieldPuns #-}

module Day8 where

import Control.Comonad
import Data.Bifunctor
import Data.Foldable
import qualified Data.Vector as V

newtype Grid a = Grid { unGrid :: V.Vector (V.Vector a) }
  deriving (Functor, Foldable, Traversable)

type Score = Int
type Count = Int
type Height = Int
type Loc = (Int, Int)

data GridZ a = GridZ
  { here :: a
  , index :: (Int, Int)
  , dir :: Dir (Maybe (GridZ a))
  }
  deriving (Functor, Foldable, Traversable)

instance Comonad GridZ where
  extract = here
  duplicate g = g' where
    g' = GridZ
      { here = g
      , index = index g
      , dir = fmap duplicate <$> dir g
      }

data Dir a = Dir { up :: a, down :: a, left :: a, right :: a }
  deriving (Functor, Foldable, Traversable, Show)

type Direction a = Dir a -> a

dirs :: Dir (Direction a)
dirs = Dir { up = up, down = down, left = left, right = right }

instance Applicative Dir where
  pure x = Dir { up = x, down = x, left = x, right = x }
  f <*> x = Dir
    { up = up f (up x)
    , down = down f (down x)
    , right = right f (right x)
    , left = left f (left x)
    }

gridz :: (Int, Int) -> Grid a -> GridZ a
gridz (i, j) g = g' where
  g' = GridZ
    { here = g `at` (i, j)
    , index = (i, j)
    , dir = (\x -> gridz <$> x <*> pure g) <$> moves
    }
  moves = Dir
    { up = if i > 0 then Just (i-1, j) else Nothing
    , down = if i < h - 1 then Just (i+1, j) else Nothing
    , right = if j < w - 1 then Just (i, j+1) else Nothing
    , left = if j > 0 then Just (i, j-1) else Nothing
    }
  h = V.length (unGrid g)
  w = V.length (unGrid g V.! 0)

at :: Grid a -> Loc -> a
at (Grid v) (i, j) = v V.! i V.! j

parse :: String -> Grid Height
parse = Grid . V.fromList . map (V.fromList . map (read . pure)) . lines

-- | Equips each point in the grid with a 4-tuple indicating for each direction
-- how tall the tallest tree is.
findTallestTrees :: GridZ Height -> GridZ (Dir Height, Height)
findTallestTrees g = g' where
  g' = GridZ
    { here = (collapse (fmap here <$> dir g'), here g)
    , index = index g
    , dir = fmap findTallestTrees <$> dir g
    }
  collapse x = (\d -> maybe (-1) (uncurry max . first d)) <$> dirs <*> x

-- | Look in each direction from the given point and return a list of elements
-- seen in each direction.
lookEverywhere :: GridZ a -> Dir [a]
lookEverywhere g = f <$> dirs <*> dir g where
  f d = maybe [] (\g -> here g : f d (d . dir $ g))

-- | Calculates for each tree T a 4-tuple indicating for each direction
-- how many trees that T can see before the edge or a tree of equal or greater height.
findViewingDistances :: GridZ Height -> GridZ (Dir Count)
findViewingDistances = extend k where
  k :: GridZ Height -> Dir Count
  k g = length . takeWhile1 (< here g) <$> lookEverywhere g where
    takeWhile1 p l = case span p l of
      (xs, []) -> xs -- when we can see all the way to the edge
      (xs, x : _) -> x : xs -- when we run into a tree, we need to count it!

scenicScore :: Dir Count -> Score
scenicScore = product

-- | A tree is visible if it is taller than the tallest tree in at least one
-- direction.
visibleTrees :: GridZ (Dir Height, Height) -> GridZ Bool
visibleTrees = fmap $ \(d, h) -> any id $ (h >) <$> d

materialize :: GridZ a -> [[a]]
materialize = rows . seekUp . seekLeft where
  seekUp x = maybe x seekUp $ up . dir $ x
  seekLeft x = maybe x seekLeft $ left . dir $ x

  row g = here g : maybe [] row (right . dir $ g)
  rows g = row g : maybe [] rows (down . dir $ g)

answer1, answer2 :: String -> Int
answer1 =
  sum . map (length . filter id) . materialize . visibleTrees . findTallestTrees . gridz (0, 0) . parse

answer2 =
  maximum . fmap maximum . materialize . fmap scenicScore . findViewingDistances . gridz (0, 0) . parse

main :: IO ()
main = print =<< answer2 <$> readFile "input/day8.txt"
