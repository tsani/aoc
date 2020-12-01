{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module P15 where

import P5 hiding ( main1, main2 )

import Control.Arrow
import           Control.Monad                  ( join
                                                , when
                                                , unless
                                                )
import Data.Bool
import Data.Functor.Identity
import           Data.Maybe                     ( listToMaybe
                                                , isNothing
                                                , catMaybes
                                                , mapMaybe
                                                , fromJust
                                                )
import Data.Foldable
import Data.Ord ( comparing )
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import StateT

data VF a = V !a !a
  deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord)
type V = VF Int

zero :: V
zero = V 0 0

(+!) :: Num a => VF a -> VF a -> VF a
V x1 y1 +! V x2 y2 = V (x1 + x2) (y1 + y2)

infixl 4 +!

(*!) :: Num a => a -> VF a -> VF a
k *! v = (k *) <$> v

infixl 6 *!

(-!) :: Num a => VF a -> VF a -> VF a
v1 -! v2 = v1 +! (-1) *! v2

manhattan :: Num a => VF a -> VF a -> a
V x1 y1 `manhattan` V x2 y2 = abs (x1 - x2) + abs (y1 - y2)

data Tile
  = Passable
  | Impassable
  | Target
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

data Dir = GoWest | GoEast | GoNorth | GoSouth
  deriving Show

dir2v :: Dir -> V
dir2v = \case
  GoWest -> V (-1) 0
  GoEast -> V 1 0
  GoNorth -> V 0 1
  GoSouth -> V 0 (-1)

v2dir :: V -> Maybe Dir
v2dir = \case
  V (-1) 0 -> Just GoWest
  V 1 0 -> Just GoEast
  V 0 1 -> Just GoNorth
  V 0 (-1) -> Just GoSouth
  _ -> Nothing

v2dir' :: V -> Dir
v2dir' v = case v2dir v of
  Nothing -> error $ "can't convert " ++ show v ++ " to a direction"
  Just x -> x

dirs :: [Dir]
dirs = [GoNorth, GoSouth, GoWest, GoEast]

data S = S
  { sGrid :: Grid
  , sCPU :: CPU
  , sV :: V -- current coordinates of the robot
  }
  deriving Show

sV' f s = s { sV = f (sV s) }
sCPU' f s = s { sCPU = f (sCPU s) }
sGrid' f s = s { sGrid = f (sGrid s) }

-- Nothing represents unknown tile.
type Grid = M.Map V (Maybe Tile)

-- | Updates the tile at the given location in the grid, only if it is unknown.
-- Updates adjacent tiles as unknown, unless they are already known.
putTile :: V -> Tile -> Grid -> Grid
putTile v t g = go (M.alter (f $ Just t) v g) (map (v +!) $ dir2v <$> dirs) where
  f t (join -> m) = case m of
    Nothing -> Just t
    Just _ -> Just m

  go = foldr (M.alter (f Nothing))

newtype Robot a = Robot { unRobot :: StateT S Identity a }
  deriving (Functor, Applicative, Monad)

instance MonadState Robot where
  type State Robot = S
  get = Robot get
  put = Robot . put

type M m = (MonadState m, State m ~ S)

runRobot :: Robot a -> S -> (S, a)
runRobot (Robot f) s = runIdentity $ unState f s

loadState :: String -> IO S
loadState path = do
  sCPU <- loadCPU path
  let sV = zero
  let sGrid = putTile sV Passable M.empty
  pure S { .. }

-- | Finds the coordinates of the unexplored tile nearest to the
-- robot. If there are no unexplored tiles, returns Nothing.
-- A manhattan distance is calculated to find the nearest tile.
nearestUnexplored :: M m => m (Maybe V)
nearestUnexplored = do
  v <- gets sV
  -- get the coordinates of all unexplored tiles, and find the one
  -- nearest to us
  fmap (minimumBy (distanceTo v)) . N.nonEmpty . map fst . filter (isNothing . snd) . M.toList <$> gets sGrid
  where
    distanceTo v = comparing (manhattan v)

-- | Gets the coordinates of the neighbours of the given coordinates.
neighbours :: V -> [V]
neighbours = traverse ((+!) . dir2v) dirs

-- | Gets coordinates of all passable tiles adjacent to the given
-- coordinates.
passableNeighbours :: Grid -> V -> [V]
passableNeighbours g =
  map fst . filter ((Passable ==) . snd) .
  mapMaybe (sequence . (id &&& (join . flip M.lookup g))) .
  neighbours

-- | Computes a sequence of directions to move the robot in the given
-- grid to move from point A to point B.
-- Because the grid's nodes have uniform cost, we can simplify
-- Dijkstra's algorithm down to a breadth-first search.
-- Returns Nothing if point B cannot be reached.
pathfind :: Grid -> V -> V -> Maybe [V]
pathfind g src dst = go S.empty [src] where
  go visited [] = Nothing -- if the frontier is empty, then we fail
  go visited (v : vs)
    -- avoid loops
    | v `S.member` visited = go visited vs
    -- if we're at the destination, then we're done
    | v == dst = Just []
    -- do a one-step lookhead
    | [v'] <- filter (dst ==) (neighbours v) = Just [v']
    -- enumerate neighbours we can traverse and add them to the frontier
    | otherwise = (v :) <$> go (v `S.insert` visited) (vs ++ ns) where
        ns = passableNeighbours g v

path2dirs :: [V] -> [Dir]
path2dirs ps = v2dir' <$> zipWith (-!) ps (src : ps)

-- | Execute commands on the robot's computer.
interp :: M m => Interp a -> m (Either Status a)
interp f = do
  (cpu', x) <- runInterp f <$> gets sCPU
  modify (sCPU' (const cpu'))
  pure x

-- | Moves the robot in the given direction.
move :: M m => Dir -> m ()
move d = interp go >>= \case
  Left e -> error (show e)
  Right (output2tile -> t) -> do
    v <- gets sV
    let v' = v +! dir2v d
    -- only move the robot when we don't hit a wall
    unless (t == Impassable) $
      modify (sV' (const v'))

    modify (sGrid' $ putTile v' t)
  where
    go = queueInput (dir2input d) *> pump

    -- | Convert a direction to an integer the computer understands.
    dir2input :: Dir -> Int
    dir2input = \case
      GoNorth -> 1
      GoSouth -> 2
      GoWest -> 3
      GoEast -> 4

    output2tile :: Int -> Tile
    output2tile = \case
      0 -> Impassable
      1 -> Passable
      2 -> Target

explore :: M m => m ()
explore = do
  nearestUnexplored >>= \case
    Nothing -> error "no more unexplored tiles"
    Just v -> pathfind <$> gets sGrid <*> gets sV <*> pure v >>= \case
      Nothing -> error "pathfind failed"
      Just ds -> traverse_ move ds

main1 :: IO ()
main1 = do
  s <- loadState "inputs/p15.txt"
  print $ runRobot explore s

-- IDEA:
-- * Use a map from coordinates to tiles.
-- * Want to find the tile nearest to the robot that is unexplored.
-- * Move the robot to that tile (use BFS to pathfind)
-- * Move the robot onto that location and update the map with the
--   result (passable, impassable, oxygen system)

-- | Starting map
start = putTile zero Passable M.empty
