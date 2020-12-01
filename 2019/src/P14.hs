{-# LANGUAGE RecordWildCards #-}

module P14 where

import Control.Applicative
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as M
import Data.Maybe ( fromJust, fromMaybe, isJust )
import qualified Data.Set as S

import Parser

-- | Maps each chemical to the reaction that produces it.
type Graph = M.Map Chem Rx

-- | Computes the list of chemicals the given chemical depends on.
deps :: Graph -> Chem -> [Chem]
deps g c = M.keys $ rxInputs $ r where
  r = case M.lookup c g of
    Nothing -> error $ "can't find " ++ c
    Just r' -> r'

-- | Computes the list of chemicals that the given chemical is required in.
revDeps :: Graph -> Chem -> [Chem]
revDeps g c = map fst $ filter f $ M.toList g where
  f :: (Chem, Rx) -> Bool
  f (_, Rx {..}) = isJust $ M.lookup c rxInputs

-- | @deleteEdge g (c, c')@ deletes the dependency of @c@ on @c'@.
deleteDep :: Graph -> (Chem, Chem) -> Graph
deleteDep g (c, c') = M.adjust f c' g where
  f :: Rx -> Rx
  f r = r { rxInputs = M.delete c (rxInputs r) }

-- | A reaction has a number of inputs, represented as a list of
-- demands, and an output.
data Rx =
  Rx
  { rxOutputCount :: !Int
  , rxInputs :: !Demands
  }
  deriving (Show, Read, Eq, Ord)

type Demands = M.Map Chem Int

type Chem = String


-- | To start, the demand table should have only @FUEL -> 1@.
-- Idea:
-- * Look up the head of the chemical list in the demand table to see
--   how much of it we need to make.
-- * Look up the reaction in the reaction table, and calculate how
--   many times we need to run the reaction.
-- * Add the calculated demands for all the inputs to the reaction to
--   the demands table, and recurse.
-- * After traversing the whole list, the demand table will contain
--   the amount of ore needed.
demands :: [Chem] -> Graph -> Demands -> Demands
demands [] _ d = d
demands (c : cs) g d = demands cs g d' where
  d' = foldr f d (M.toList rxInputs) where
    f (c', k) = M.alter (Just . (+ (k * timesToRun)) . fromMaybe 0) c'
    -- look up required amount of the chemical and the reaction to
    -- produce the chemical.
    required = d M.! c
    Rx { .. } = g M.! c
    -- calculate the number of times to run the reaction by dividing
    -- the required amount of the chemical by the amount produced per
    -- reaction, adding one if the division isn't even.
    timesToRun = q + signum r where
      (q, r) = required `quotRem` rxOutputCount

-- | Topologically sorts the graph. This is the reverse order the
-- graph should be traversed in.
-- (Implementation is essentially Kahn's algorithm.)
topo :: Graph -> [Chem]
topo = go (S.singleton "ORE") where
  go :: S.Set Chem -> Graph -> [Chem]
  go chems g = case S.minView chems of
    Just (c, chems') ->
      c : uncurry go (foldr (curry f c) (chems', g) (revDeps g c))
    Nothing -> []

  f :: (Chem, Chem) -> (S.Set Chem, Graph) -> (S.Set Chem, Graph)
  f (c, c') (chems, g) =
    if null (deps g' c') then
      (S.insert c' chems, g')
    else
      (chems, g')
    where
      g' = deleteDep g (c, c')

-- | Adds an ORE key to the graph with no inputs.
withOre :: Graph -> Graph
withOre = M.insert "ORE" Rx { rxOutputCount = 1, rxInputs = M.empty }

parseGraph :: String -> Maybe Graph
parseGraph = flip parse (graph <* eof) where
  graph = M.fromList <$> many (reaction <* char '\n')

  reaction = do
    rxInputs <- M.fromList . N.toList <$> product `sepBy1` (string ", ")
    string " => "
    (c, rxOutputCount) <- product
    pure $ (c, Rx { .. })

  product = do
    k <- integer
    _ <- char ' '
    c <- some upper
    pure (c, k)

-- | For a given reaction graph, calculate the about of ore needed to
-- make n FUEL.
answer1 :: Graph -> Int -> Int
answer1 g n = demands (reverse $ topo g) g (M.singleton "FUEL" n) M.! "ORE"

load :: IO Graph
load = fromJust . fmap withOre . parseGraph <$> readFile "inputs/p14.txt"

main1 :: IO ()
main1 = do
  g <- load
  print $ answer1 g 1

trillion :: Int
trillion = 1000000000000

-- | Performs binary search.
-- @binsearch (lo, hi) f p@ requires that @p (f lo)@ be @False@ and @p
-- (f hi)@ be @True@ for the search to continue. If either of these
-- gives the "wrong" answer, then the search is over.
binsearch :: (Int, Int) -> (Int -> a) -> (a -> Bool) -> [(Int, a)]
binsearch (lo, hi) f p
  | p (f lo) = []
  | not (p (f hi)) = []
  | otherwise = -- then p (f lo) is False and p (f hi) is True.
    case () of
      _ | mid == lo || mid == hi -> [] -- loop detected
      _ ->
        (mid, f mid) :
        if p (f mid) then
          binsearch (lo, mid) f p
        else
          binsearch (mid, hi) f p
    where
      mid = (lo + hi) `div` 2


answer2 :: Graph -> Int
answer2 g = fst $ last $ binsearch (1, 1000000000000) (answer1 g) (> trillion)

main2 :: IO ()
main2 = do
  g <- load
  print $ answer2 g
