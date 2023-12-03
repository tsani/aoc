{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

module Day3 where

import Data.Char
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Data.Vector (Vector(..))
import Data.Vector qualified as V
import Control.Comonad.Representable.Store (Store(..), StoreT(..), runStore, store, experiment)
import Control.Comonad(Comonad(..))

import Parser

-- following: https://chrispenner.ca/posts/conways-game-of-life

-- MODEL ----------------------------------------------------------------------

newtype VBounded a = VBounded { unBounded :: Vector (Vector a) }
  deriving (Eq, Ord, Show, Foldable, Functor)

type Digit = Int

data Square = Blank | Digit Digit | Symbol Char
  deriving (Eq, Ord, Show)

isDigitSquare (Digit _) = True
isDigitSquare _ = False

isSymbolSquare (Symbol _) = True
isSymbolSquare _ = False

type Input = VBounded Square

gridSize :: Int
gridSize = 140

instance Distributive VBounded where
  distribute = distributeRep

instance Representable VBounded where
  type Rep VBounded = (Int, Int)
  index (VBounded g) (i, j)
    -- | (0 <= i && i < gridSize) && (0 <= j && j < gridSize)
      = g V.! (i `mod` gridSize) V.! (j `mod` gridSize)
    -- | otherwise = error "out of bounds"
  tabulate desc = VBounded $
    V.generate gridSize $ \i ->
    V.generate gridSize $ \j ->
    desc (i, j)

type SymbolAdjacent = Bool

type Grid a = Store VBounded a

vboundedToGrid :: VBounded a -> Grid a
vboundedToGrid (VBounded v) = store (\(i, j) -> v V.! i V.! j) (0, 0)

-- make sure not to go out of bounds!
north, south, east, west, northeast, northwest, southeast, southwest :: Grid a -> Grid a
west g = store v (i, j-1) where
  (v, (i, j)) = runStore g
east g = store v (i, j+1) where
  (v, (i, j)) = runStore g
north g = store v (i-1, j) where
  (v, (i, j)) = runStore g
south g = store v (i+1, j) where
  (v, (i, j)) = runStore g
northeast = east . north
northwest = west . north
southeast = east . south
southwest = west . south

neighbours :: [Grid a -> Grid a]
neighbours = [north, south, east, west, northeast, southeast, northwest, southwest]

-- | Decides whether the current focus of the grid is in bounds.
inBounds :: Grid a -> Bool
inBounds g = (0 <= i && i < gridSize) && (0 <= j && j < gridSize) where
  (_, (i, j)) = runStore g

-- SOLUTION -------------------------------------------------------------------

-- | A cell C is a _leader_ when it satisfies the following condition:
-- - It does not have a digit on the left
-- - It n>=0 digits on the right
-- Taken as a whole, this is a multi-digit number now, which has a boundary.
-- The cell is a leader if this boundary contains at least one symbol.
--
-- `solve` replaces all cells with the value of the leader at that
-- position, else zero.
solve :: Grid (SymbolAdjacent, Square) -> Grid Int
solve = extend rule where
  rule :: Grid (SymbolAdjacent, Square) -> Int
  rule g = case snd $ extract g of
    Blank -> 0
    Symbol _ -> 0
    Digit _ | digitOnLeft -> 0
    Digit _ | symbolAdjacentToNumber -> representedNumber
    Digit _ | otherwise -> 0
    where
      (_, (i, j)) = runStore g -- get the coords of the cell under consideration

      digitOnLeft = not onLeftBoundary
        && isDigitSquare (snd $ extract (west g))

      onLeftBoundary = j == 0

      (symbolAdjacentToNumber, representedNumber) = scanEast g (False, 0)

      scanEast :: Grid (SymbolAdjacent, Square) -> (SymbolAdjacent, Int) -> (SymbolAdjacent, Int)
      scanEast g (!symAdj, !acc)
        | j >= gridSize = (symAdj, acc)
        | j < gridSize = case extract g of
          (symAdj', Digit d) ->
            scanEast (east g) (symAdj' || symAdj, 10*acc + d)
          _ -> (symAdj, acc)

checkSymbolAdjacent :: Grid Square -> Grid (SymbolAdjacent, Square)
checkSymbolAdjacent = extend rule where
  rule g = (adj, x) where
    x = extract g
    adj = any (isSymbolSquare . extract) . filter inBounds $ map ($ g) neighbours

answer1 :: Input -> Int
answer1 =
  V.sum .
  V.map V.sum .
  unBounded .
  tabulate @VBounded .
  fst .
  runStore .
  solve .
  checkSymbolAdjacent .
  vboundedToGrid

-- PARSING --------------------------------------------------------------------

parse :: String -> Input
parse = VBounded . V.fromList . map (V.fromList . map square) . lines where
  square = \case
    '.' -> Blank
    c | isDigit c -> Digit (read $ pure c)
    c | isSymbol c || isPunctuation c -> Symbol c
    c -> error $ "unknown character: " ++ show c

-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = print =<< answer1 . parse <$> readFile "input/day3.txt"
