{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

module Day3 where

import Control.Arrow
import Control.Comonad.Representable.Store (Store(..), StoreT(..), runStore, store, experiment)
import Control.Comonad(Comonad(..))
import Data.Char
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Data.List (nub)
import Data.Vector (Vector(..))
import Data.Vector qualified as V

-- following: https://chrispenner.ca/posts/conways-game-of-life

-- MODEL ----------------------------------------------------------------------

newtype VBounded a = VBounded { unBounded :: Vector (Vector a) }
  deriving (Eq, Ord, Show, Foldable, Functor)

type Digit = Int

data BasicSquare = BasicBlank | BasicDigit Digit | BasicSymbol Char
  deriving (Eq, Ord, Show)

data Square = Blank | Digit { digit :: Digit, number :: Int, tag :: Int } | Symbol Char

isDigitSquare (Digit {}) = True
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

-- SOLUTION TO PART 1 ---------------------------------------------------------

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
    Digit {} | digitOnLeft -> 0
    Digit { number } | symbolAdjacentToNumber -> number
    Digit {} | otherwise -> 0
    where
      (_, (i, j)) = runStore g -- get the coords of the cell under consideration

      digitOnLeft = not onLeftBoundary
        && isDigitSquare (snd $ extract (west g))

      onLeftBoundary = j == 0

      symbolAdjacentToNumber = scanEast g False

      scanEast = foldEast $ \sq !symAdj -> case sq of
        (symAdj', Digit _ _ _) -> Just (symAdj' || symAdj)
        (_, _) -> Nothing

-- | Fold over the elements of the grid at the current position going east.
--This is kind of unusual because we want to be able to signal when to stop, so
--I threw in a Maybe.
foldEast :: (a -> b -> Maybe b) -> Grid a -> b -> b
foldEast f g !acc
  | j >= gridSize = acc
  | j < gridSize = case f (extract g) acc of
    Nothing -> acc
    Just acc -> foldEast f (east g) acc
  where
    (_, (i, j)) = runStore g

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
  tabulate .
  fst .
  runStore .
  solve .
  checkSymbolAdjacent .
  vboundedToGrid

-- SOLUTION TO PART 2 ---------------------------------------------------------

-- I can reuse part of part 1 for this, can't I?
-- In the grid where we found all the leaders
-- Maybe it's best not to.
-- What if I find all the numbers in the grid _and numbered them?_
-- Basically doing clustering on the digits and assigning a label to each
-- cluster.
-- Then for each gear I can just look at how many unique labels are among its
-- neighbours!
-- Actually fuck that. Let's just use the fact that next to a * we never have
-- the same number twice. Therefore, we can use the value of each part number
-- as the unique tag itself.

solve2 :: Grid Square -> Grid Int
solve2 = extend rule where
  -- idea: map each '*' in the grid to its gear ratio and everything else to 0
  rule :: Grid Square -> Int
  rule g
    | Symbol '*' <- extract g
    , [n1, n2] <- neighbourValues = n1 * n2
    | otherwise = 0
    where
      neighbourValues =
        nub .
        filter (>0) .
        map (digitValue . extract . ($ g)) $
        neighbours

      digitValue (Digit { number }) = number
      digitValue _ = 0

answer2 :: Input -> Int
answer2 =
  V.sum . V.map V.sum .
  unBounded . tabulate .
  fst . runStore .
  solve2 .
  vboundedToGrid

-- PARSING --------------------------------------------------------------------

preprocess :: [[BasicSquare]] -> Input
preprocess = VBounded . V.fromList . map (V.fromList . fst . go 0) where
  -- idea: loop over the line to parse the digits into numbers
  -- keep an accumulator that we use to build up the number
  -- but we can only tag the current digit after seeing what's on the right!
  -- So we return not only the rest of the list, parsed, but also the number
  go acc [] = ([], acc)
  go acc (c:cs) = case c of
    BasicBlank -> (Blank : fst (go 0 cs), acc)
    BasicSymbol c -> (Symbol c : fst (go 0 cs), acc)
    BasicDigit digit -> case go (10*acc + digit) cs of
      (rest, number) -> (Digit { digit, number } : rest, number)

parse :: String -> [[BasicSquare]]
parse = map (map square) . lines where
  square = \case
    '.' -> BasicBlank
    c | isDigit c -> BasicDigit (read $ pure c)
    c | isSymbol c || isPunctuation c -> BasicSymbol c
    c -> error $ "unknown character: " ++ show c

-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = print =<< (answer1 &&& answer2) . preprocess . parse <$> readFile "input/day3.txt"
