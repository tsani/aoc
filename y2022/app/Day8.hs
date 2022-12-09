{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, NamedFieldPuns #-}

module Day8 where

import qualified Data.Vector as V

newtype Grid a = Grid { unGrid :: V.Vector (V.Vector a) }
  deriving (Functor, Foldable, Traversable)

type Height = Int
type Loc = (Int, Int)

data GridZ a = GridZ
  { here :: a
  , index :: (Int, Int)
  , dir :: Dir (Maybe (GridZ a))
  }
  deriving (Functor, Foldable, Traversable)

data Dir a = Dir { up :: a, down :: a, left :: a, right :: a }
  deriving (Functor, Foldable, Traversable, Show)

dirs :: Dir (Dir a -> a)
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
    { here = (collapse (dir g'), here g)
    , index = index g
    , dir = fmap findTallestTrees <$> dir g
    }
  f access (d, h) = max (access d) h
  collapse x = ((\d -> maybe (-1) (f d)) <$> dirs) <*> (fmap here <$> x)

visibleTrees :: GridZ (Dir Height, Height) -> GridZ Bool
visibleTrees = fmap $ \(d, h) ->
  any (\dir -> h > dir d) [up, down, left, right]

materialize :: GridZ a -> [[a]]
materialize = rows . seekUp . seekLeft where
  seekUp x = maybe x seekUp $ up . dir $ x
  seekLeft x = maybe x seekLeft $ left . dir $ x

  row g = here g : maybe [] row (right . dir $ g)
  rows g = row g : maybe [] rows (down . dir $ g)

main :: IO ()
main =
  print
  =<< sum . map (length . filter id) . materialize . visibleTrees . findTallestTrees . gridz (0, 0) . parse
  <$> readFile "input/day8.txt"
