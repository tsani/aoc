{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Day20 where

import Control.DeepSeq
import Data.Bifunctor
import Data.Bits hiding ( rotate )
import Data.Coerce
import Data.Function ( on )
import Data.List ( uncons )
import qualified Data.List.NonEmpty as N
import Data.Maybe ( catMaybes )
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

import Debug.Trace

import GHC.Generics

import BiMap ( BiMap )
import qualified BiMap as B
import Comonad
import Util ( fixBy, readText )
import ZZ ( ZZ )
import qualified ZZ

-- ----- EDGES and TILES

newtype Edge = Edge Word16
  deriving (Eq, Ord, Show)
  deriving newtype NFData

class Reversible a where
  rev :: a -> a

-- | Quickly reverses an edge using the lookup table.
instance Reversible Edge where
  rev (Edge i) = reverses V.! (fromIntegral i) where
    -- | A precalculated table of all 10-bit reversed edges.
    reverses :: Vector Edge
    reverses = V.generate (2^10) (reverseEdge . Edge . fromIntegral) where
      reverseEdge :: Edge -> Edge
      reverseEdge (Edge w) =
        Edge $ foldr (.|.) 0 [bit (9 - i) | i <- [0..9], testBit w i]

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
data Tile' a = Tile { tLeft :: !a, tTop :: !a, tRight :: !a, tBottom :: !a }
  deriving (Eq, Ord, Functor, Show, Generic, NFData)

type Tile = Tile' Edge

newtype Side = Side { unSide :: forall a. Tile' a -> a }

edges :: Tile' a -> [a]
edges (Tile left top right bottom) = [left, top, right, bottom]

-- ----- ROTATIONS

newtype Rotation' a = CW Int
  deriving (Eq, Ord, Show)
  deriving newtype NFData

-- | Interprets a rotation into a transformation of tiles.
rotate :: Rotation' a -> Tile' a -> Tile' a
rotate (CW k)
  | k >= 0 = (!! k) . iterate cw
  | k < 0 = (!! (-k)) . iterate ccw
  | otherwise = error "bogus rotation"
  where
  cw (Tile left top right bottom) = Tile bottom left top right
  ccw (Tile left top right bottom) = Tile top right bottom left

type Rotation = Rotation' Edge

instance Semigroup (Rotation' a) where
  CW f <> CW g = CW $ f + g `mod` 4

instance Monoid (Rotation' a) where
  mempty = CW 0

clockwise, counterclockwise :: Rotation' a
clockwise = CW 1
counterclockwise = CW (-1)

-- ----- FLIPS

data Flip' a = Flip { fHoriz :: Bool, fVert :: Bool } deriving (Generic, NFData, Show, Eq)

-- | Interprets a flip into a transformation of tiles.
flip :: Reversible a => Flip' a -> Tile' a -> Tile' a
flip (Flip h v) = (if h then horiz else id) . (if v then vert else id) where
  horiz (Tile left top right bottom) =
    Tile (rev right) (rev top) (rev left) (rev bottom)
  vert (Tile left top right bottom) =
    Tile (rev left) (rev bottom) (rev right) (rev top)

-- newtype Flip' a = Flip { flip :: Tile' a -> Tile' a }
--   deriving newtype NFData

type Flip = Flip' Edge

horizontally, vertically :: Flip' a
horizontally = Flip True False
vertically = Flip False True

instance Semigroup (Flip' a) where
  Flip h1 v1 <> Flip h2 v2 = Flip (h1 /= h2) (v1 /= v2)

instance Monoid (Flip' a) where
  mempty = Flip False False

data Transf' a = Transf !(Flip' a) !(Rotation' a) deriving (Generic, NFData, Show, Eq)

instance Semigroup (Transf' a) where
  Transf f1 r1 <> Transf f2 r2 = Transf (f1 <> f2) (r1 <> r2)

instance Monoid (Transf' a) where
  mempty = Transf mempty mempty

type Transf = Transf' Edge

-- | Applies a transformation to a tile.
transf :: Reversible a => Transf' a -> Tile' a -> Tile' a
transf (Transf f r) = Day20.flip f . rotate r

type Grid = ZZ (Maybe (IdTile, Transf))

-- | Suppose @(t, s) `align` (t', s')@ = (f, r)@. Then,
-- the edge on side @s'@ of @(rotate r . flip f) t'@ matches the edge
-- on side @s@ of @t@.
-- WARNING: align is terminating only on input tiles with (at least)
-- one compatible edge.
align
  :: (Eq a, Reversible a) => Show a
  => Flip' a
  -> (Tile' a, Side) -> (Tile' a, Side)
  -> Transf' a
align f (t1, Side side1) (t2, Side side2) = go 5 t2 where
  -- Because we never need more than a few rotations, we put a limit
  -- of 5 recursive calls to catch nontermination.
  go 0 _ = error $
    "align ran out of gas! for\n" ++
    show t1 ++ "\nand\n" ++ show t2
  go n t2
    | side1 t1 == rev (side2 t2) = mempty
    | side1 t1 == side2 t2 = Transf f mempty
    | otherwise = Transf mempty clockwise <> go (n - 1) (rotate clockwise t2)

-- | Given a tile-edge bipartite graph and a nonempty upper triangular
-- grid, extend the grid by one, unless it is full.  The fixed point
-- of this function is a full grid.  A grid is _nonempty upper
-- triangular_ if it has at least one Just entry, and all its Just
-- entries form a triangle starting in the top left corner of the
-- grid.
step :: BiMap IdTile Edge -> Grid -> Grid
step b zz = zz `cobind` k where
  k :: ZZ (Maybe (IdTile, Transf)) -> Maybe (IdTile, Transf)
  k z = case (extract z, extract =<< ZZ.up z, extract =<< ZZ.left z) of
    -- if we already filled in this tile, do nothing.
    (Just _, _, _) -> extract z

    -- In this case, we're looking at a tile that isn't on the
    -- frontier, so we can't do anything yet.
    (Nothing, Nothing, Nothing) -> Nothing

    -- if the tile _above_ is (in bounds and) filled in, then we can look
    -- up its _bottom_ edge in the map to find the tile that needs to
    -- go here.
    (Nothing, Just itphi, _) ->
      Just (match b itphi horizontally (Side tBottom) (Side tTop))

    -- Same idea in case we need to look at the _left_ tile.
    (Nothing, Nothing, Just itphi) ->
      Just (match b itphi vertically (Side tRight) (Side tLeft))

  -- @match (t, phi) side side' = (t', psi)@ such that @t /= t'@
  -- and @side@ of @transf t phi@ matches @side'@ of @transf t' psi@.

-- | Finds a tile and an alignment for it that matches a given aligned
-- tile. The alignment is done on the given sides
match
  :: BiMap IdTile Edge -> (IdTile, Transf) -> Flip -> Side -> Side
  -> (IdTile, Transf)
match b ((i, t), phi) f side side' =
  let e = unSide side $ transf phi t in
  case B.lookupB e b of
    x | 2 /= S.size x ->
        error $ show e ++ " is matched by /= 2 tiles " ++ concatMap (("\n" ++) . show) (S.toList x)

    -- When we look up this edge, because we know it is between
    -- exactly two tiles, one of which is `t`, we need to filter the
    -- result to find the new tile.
    (filter ((i, t) /=) . S.toList -> [(i', t')]) ->
      -- Then there must be exactly one tile left, and we need to
      -- align it.
      -- Because t and t' are related by an edge (namely e), they satisfy the
      -- precondition on `align`.
      ( (i', t')
      , align f (transf phi t, side) (t', side')
      )

newtype TileId = TileId { unTileId :: Int }
  deriving (Eq, Ord, Show)
  deriving newtype NFData

type IdTile = (TileId, Tile)

type RawTile = (TileId, [[Bool]])

fromBits :: [Bool] -> Edge
fromBits = Edge . foldr (.|.) 0 . map ((shiftL 1) . fst) . filter snd . zip [0..]

toBiMap :: [IdTile] -> BiMap IdTile Edge
toBiMap = foldr f B.empty where
  f (i, t) b = foldr (B.insert (i, t)) b (edges t ++ map rev (edges t))

-- | A corner tile has exactly two lonely edges.
newtype CornerTile = CornerTile (Tile' (Bool, Edge)) deriving Show

-- | Forgets that a tile is a corner tile.
forgetCorner :: CornerTile -> Tile
forgetCorner (CornerTile t) = snd <$> t

-- | Produces a transformation that reorients the given corner tile to
-- be the top left corner.
-- If @topLeftCorner c = r@, then the lonely edges of
-- @rotate r (forgetCorner c) = t@ are left edge and top edge.
topLeftCorner :: CornerTile -> Rotation
topLeftCorner (CornerTile t) = go t where
  go (Tile (True, _) (True, _) _ _) = mempty
  go t = clockwise <> go (rotate clockwise t)

-- | Finds all tiles with two lonely edges each.
-- The returned list of tiles' edges is augmented with whether that
-- edge is lonely.
corners :: BiMap IdTile Edge -> [(TileId, CornerTile)]
corners b = coerce $ filter f $ map g (B.left b) where
  f = (2 ==) . length . filter fst . edges . snd
  g = second (fmap (\e -> (isLonely b e, e)))

-- | Decides if an edge is lonely in the graph.
-- An edge is _lonely_ if it belongs to exactly one tile.
isLonely :: BiMap IdTile Edge -> Edge -> Bool
isLonely b e = 1 == S.size (B.lookupB e b)

encodeTile :: [[Bool]] -> Tile' [Bool]
encodeTile ts = Tile left top right bottom where
  left = Prelude.reverse $ map head ts
  top = head ts
  right = map last ts
  bottom = reverse $ last ts

parse :: Text -> [IdTile]
parse = map (second (fmap fromBits . encodeTile) . parseTile) . T.splitOn "\n\n" where
  parseTile :: Text -> RawTile
  parseTile (uncons . T.lines -> Just (h, t)) =
    ( TileId (readText (T.init $ T.words h !! 1))
    , map ('#' ==) . T.unpack <$> t
    )

p1 :: IO ()
p1 = print . go . parse =<< T.readFile "input/day20.txt" where
  go l = p $ ZZ.corners $ (fmap (unTileId . fst . fst)) $ head $ catMaybes $ map sequenceA $ grids where
    p (i1, i2, i3, i4) = i1 * i2 * i3 * i4
    b = toBiMap l
    ic : _ = corners b
    -- make an empty grid and fill in the top left corner the the
    -- appropriately arranged tile.
    grids = iterate (step b) (initialGrid ic)

initialGrid :: (TileId, CornerTile) -> Grid
initialGrid (i, c) = const (Just ((i, forgetCorner c), Transf mempty r)) `ZZ.modify` emptyGrid 12 12 where
  r = topLeftCorner c

-- | Constructs an empty grid with the given width and height.
emptyGrid :: Int -> Int -> ZZ (Maybe a)
emptyGrid w h = ZZ.fromList $ N.fromList (replicate h (N.fromList (replicate w Nothing)))
