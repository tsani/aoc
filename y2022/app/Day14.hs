{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, RankNTypes, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE LambdaCase, TypeApplications #-}

module Day14 where

import Parser
import Grid ( KnownSize, Grid, GridZ )
import qualified Grid as G

import Control.Applicative
import Control.Monad
import Data.Distributive
import Data.Foldable
import Data.List (unfoldr)
import Data.Maybe ( fromJust )
import qualified Data.Set as S
import qualified Data.Vector as V

data Pair a = Pair a a
  deriving (Functor, Traversable, Foldable, Eq, Ord, Show)

pfst, psnd :: Pair a -> a
pfst (Pair x _) = x
psnd (Pair _ x) = x
selectors :: Pair (Pair a -> a)
selectors = Pair pfst psnd

instance Distributive Pair where
  distribute f = fmap fmap selectors <*> pure f

dup :: a -> Pair a
dup x = Pair x x

p2t :: Pair a -> (a, a)
p2t (Pair x y) = (x, y)

instance Applicative Pair where
  pure = dup
  Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

type Pos = Pair Int
type Path = [Pos]

data Input = Input { unInput :: [Path] }

parse :: String -> Input
parse = Input . map parseLine . lines where
  parseLine :: String -> Path
  parseLine s = snd . fromJust . flip runParser s $ tuple `sepBy` string " -> " where
    tuple = Pair <$> (number <* char ',') <*> number

-- | Expands a pair of positions into a list of all the positions between them.
-- This only works for positions that are horizontally or vertically aligned.
expand :: Pos -> Pos -> [Pos]
expand p1 p2
  | p1 == p2 = []
  | otherwise = p1 : expand ((+) <$> p1 <*> (signum <$> ((-) <$> p2 <*> p1))) p2

-- | Expands a whole path into a list of all the positions between each pair of
-- points.
expandPath :: Path -> [Pos]
expandPath [] = []
expandPath [p] = [p]
expandPath (p1:p2:ps) = expand p1 p2 ++ expandPath (p2:ps)

-- | Expands the whole input into a list of points where there is rock.
expandInput :: Input -> [Pos]
expandInput = concatMap expandPath . unInput

data Tile = Air | Sand | Rock
  deriving (Eq, Ord, Show)

t2c :: Tile -> Char
t2c = \case
  Air -> ' '
  Sand -> 'o'
  Rock -> '#'

passable :: Tile -> Bool
passable = (Air ==)

blocked :: Tile -> Bool
blocked = not . passable

type SandGrid h w = GridZ h w Tile

-- | Initializes a grid with Rock everywhere indicated by the input.
-- The grid is initially positioned at (500, 0) (in problem coordinates), where
-- the sand starts falling.
setup :: Input -> (forall h w. KnownSize h w => SandGrid h w -> r) -> r
setup input k = G.sizedGrid v (k . G.gridz (G.fin' 0, G.fin' $ 500 - pfst offset)) where
  s = S.fromList (expandInput input)
  Pair offset maxs =
    (Pair (pure minimum <*>) (pure maximum <*>)) <*> (pure $ distribute $ expandInput input)

  Pair width _ = (-) <$> maxs <*> offset
  v = V.generate (psnd maxs + 1) $ \j -> V.generate (width + 1) $ \i ->
    if Pair (pfst offset + i) j `S.member` s then Rock else Air


-- | Given a Grid positioned where sand spawns, simulate its motion, moving the grid focus.
-- This process stops when the sand stops and the grid is returned.
-- If the sand would fall out of bounds, returns Nothing.
sand :: forall h w. KnownSize h w => SandGrid h w  -> Maybe (SandGrid h w)
sand g = do
  !d <- down g
  !dl <- down <=< left $ g
  !dr <- down <=< right $ g
  case () of
    _ | passable' d -> sand d
      | passable' dl -> sand dl
      | passable' dr -> sand dr
    _ -> Just g
  where
    passable' = passable . G.extract
    down = G.down . G.dir
    left = G.left . G.dir
    right = G.right . G.dir

-- | Replace the tile under focus with Sand.
freeze :: KnownSize h w => SandGrid h w -> SandGrid h w
freeze g = (`G.extend` g) $ \g' -> if G.pos g == G.pos g' then Sand else G.extract g'

-- | Given a grid positioned where sand spawns, simulate the motion of a new grain of sand.
-- If the sand would fall out of bounds, returns Nothing.
-- Otherwise, returns a grid in which the sand has stopped and the grid position
-- returned to the start.
step :: KnownSize h w => SandGrid h w -> Maybe (SandGrid h w)
step g = G.seek (G.pos g) . freeze <$> sand g

-- | Given a grid positioned where sand spawns, repeatedly spawn sand and
-- simulate its motion. Computes a sequence of grids, each of which has one more
-- grain of sand resting in it that the previous grid.
-- The grid at index i has i grains of sand in it.
-- The last element of the grid is such that spawning one more grain of sand and
-- letting it fall would cause that grain to fall out of bounds.
simulate :: KnownSize h w => SandGrid h w -> [SandGrid h w]
simulate = unfoldr (fmap (p2t . dup) . step)

countSand :: KnownSize h w => SandGrid h w -> Int
countSand = sum . map (length . filter (Sand ==)) . G.materialize

answer1 :: Input -> Int
answer1 input = setup input $ countSand . last . simulate

printGrid :: KnownSize h w => SandGrid h w -> IO ()
printGrid = traverse_ (putStrLn . map t2c) . G.materialize

main :: IO ()
main = print . answer1 . parse =<< readFile "input/day14.txt"

repeatedM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatedM 0 _ = pure
repeatedM n k = repeatedM (n-1) k <=< k
