{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day20 where

import Data.Bifunctor
import Data.Bits
import Data.List ( uncons )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word

import Util ( readText )

newtype Edge = Edge Word16 deriving (Eq, Show)

-- | A precalculated table of all 10-bit reversed edges.
reverses :: Vector Edge
reverses = V.generate (2^10) (reverseEdge . Edge . fromIntegral) where
  reverseEdge :: Edge -> Edge
  reverseEdge (Edge w) = Edge $ foldr (.|.) 0 [bit (9 - i) | i <- [0..9], testBit w i]

-- | Quickly reverses an edge using the lookup table.
rev :: Edge -> Edge
rev (Edge i) = reverses V.! (fromIntegral i)

-- | Represents the edges of a tile of the picture to reassemble.
-- The representation is _clockwise_, which means rotations just
-- rotate the edges without touching their data.
-- For example, consider this 5x5 tile:
-- @@
-- #.##.
-- .   #
-- .   .
-- .   #
-- #..#.
-- @@
-- It is represented as (using strings for simplicity)
-- Tile { tTop = "#.##.", tRight = ".#.#.", tBottom = ".#..#", tLeft = "#...#" }
-- Each edge is actually represented as an unsigned integer, with a 0
-- bit for empty and a 1 bit for occupied.
data Tile' a = Tile !a !a !a !a
  deriving (Eq, Functor, Show)

type Tile = Tile' Edge

newtype Rotation a = Rotation { rotate :: Tile' a -> Tile' a }

edges :: Tile -> [Edge]
edges (Tile left top right bottom) = [left, top, right, bottom]

{-
clockwise, counterclockwise :: Rotation a
clockwise = Rotation $ \(Tile left top right bottom) -> Tile bottom left top right
counterclockwise = Rotation $ \(Tile left top right bottom) -> Tile top right bottom left

newtype Flip = Flip { flip :: Tile -> Tile }

horizontally, vertically :: Flip
horizontally = Flip $ \(Tile left top right bottom) -> Tile (rev right) (rev top) (rev left) (rev bottom)
vertically = Flip $ \(Tile left top right bottom) -> Tile (rev left) (rev bottom) (rev right) (rev top)
-}

-- idea: find all tiles with two edges that don't match any other edge.
-- bad idea. Turns out it doesn't seem to work, and won't help for
-- part 2 when we need to have a fully reassembled image.
-- Instead, we can construct a Map Tile (Set Edge), the "forward map" F,
-- where the set is all the edges and all their reverses.
-- Then we invert to form the reverse map R, Map Edge (Set Tile).
-- Pick a starting tile arbitrarily and 'remove it'.
-- (To remove a tile T, for each edge E in T, remove T from R(E).
-- If R(E) becomes empty, remove E from R. Remove T from F.)

type SetMap k a = Map k (Set a)
data BiMap k1 k2 = BiMap {



-- | Counts the number of edges in the given tile that don't match any other edge in any other tile.
-- (Considers flips.)
countUnmatched :: [Tile] -> Tile -> Int
countUnmatched ts t = sum
  [ 1
  | e <- edges t
  , forAll ts $ \t' ->
      t /= t'
    && (forAll (edges t') $
        \e' -> forAll flips $ \(f1, f2) -> f1 e /= f2 e')
  ]
  where
    forAll :: Traversable t => t a -> (a -> Bool) -> Bool
    forAll = flip all

    flips = [(id, rev), (rev, id), (id, id), (rev, rev)]

type IdTile = (Int, Tile)

type RawTile = (Int, [[Bool]])

fromBits :: [Bool] -> Edge
fromBits = Edge . foldr (.|.) 0 . map ((1 `shiftL`) . fst) . filter snd . zip [0..]

encodeTile :: [[Bool]] -> Tile' [Bool]
encodeTile ts = Tile left top right bottom where
  left = reverse $ map head ts
  top = head ts
  right = map last ts
  bottom = reverse $ last ts

parse :: Text -> [IdTile]
parse = map (second (fmap fromBits . encodeTile) . parseTile) . T.splitOn "\n\n" where
  parseTile :: Text -> RawTile
  parseTile (uncons . T.lines -> Just (h, t)) =
    ( readText (T.init $ T.words h !! 1)
    , map ('#' ==) . T.unpack <$> t
    )

p1 :: IO ()
p1 = print . go . parse =<< T.readFile "input/day20-small.txt" where
  go l = [(i, countUnmatched (map snd l) t) | (i, t) <- l {- , countUnmatched (map snd l) t == 2 -}]
