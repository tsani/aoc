{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RankNTypes #-}

module Day10 where

import Control.Comonad (Comonad(..))
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict qualified as M
import Data.Vector (Vector(..))
import Data.Vector qualified as V

import Grid

data Conn = MkConn String (forall a. Direction a) (forall a. Direction a)

instance Show Conn where
  show (MkConn name _ _) = name

instance Eq Conn where
  MkConn n1 _ _ == MkConn n2 _ _ = n1 == n2

ns, ew, ne, nw, se, sw :: Conn
ns = MkConn "NS" up down
ew = MkConn "EW" left right
ne = MkConn "NE" up right
nw = MkConn "NW" up left
se = MkConn "SE" down right
sw = MkConn "SW" down left

data Tile = Ground | Start | Conn Conn
  deriving (Eq, Show)

type Input = [[Tile]]

parse :: String -> Input
parse = map (map one) . lines where
  one = \case
    '|' -> Conn ns
    '-' -> Conn ew
    'L' -> Conn ne
    'J' -> Conn nw
    '7' -> Conn sw
    'F' -> Conn se
    '.' -> Ground
    'S' -> Start
    _ -> error "wot"

answer1 :: forall h w. KnownSize h w => GridZ h w Tile -> GridZ h w (Tile, Int)
answer1 g = kfix (g =>> rule) where
  infinity = 10000

  rule :: GridZ h w Tile -> (GridZ h w (Tile, Int) -> (Tile, Int))
  rule g = case extract g of
    Start -> const (Start, 0)
    Ground -> const (Ground, infinity)
    x@(Conn (MkConn _ d1 d2)) -> \g ->
      (x, min (neighbourDistance d1 g) (neighbourDistance d2 g) + 1)

  neighbourDistance d g =
    maybe infinity snd $ extract <$> d (dir g)

main :: IO ()
main = do
  input <- parse <$> readFile "input/day10.txt"
  sizedGrid (V.fromList . map V.fromList $ input) $ \g -> do
    print $ concat $ materialize $ answer1 (gridz0 g)
