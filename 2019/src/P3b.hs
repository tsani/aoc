module P3b where

import Control.Monad ( join, guard )
import Data.List ( minimumBy, foldl' )
import Data.Coerce
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S

newtype Wire = Wire { unWire :: Int }
  deriving (Eq, Ord)

type Vec = (Int, Int)

origin :: Vec
origin = (0, 0)

(+++) :: Vec -> Vec -> Vec
(k1, k2) +++ (l1, l2) = (k1 + l1, k2 + l2)

type Grid = M.Map Vec (M.Map Wire Int)

alter :: (Maybe Int -> Maybe Int) -> Vec -> Wire -> Grid -> Grid
alter h v w g = M.alter (pure . M.alter h w . fromMaybe M.empty) v g where

wires :: [Wire]
wires = Wire <$> [0..]

data Dir = L | R | U | D
  deriving (Show)

instance Read Dir where
  readsPrec _ (d : s) =
    case d of
      'D' -> pure (D, s)
      'U' -> pure (U, s)
      'L' -> pure (L, s)
      'R' -> pure (R, s)

dirToVec :: Dir -> Vec
dirToVec d = case d of
  L -> (-1, 0)
  R -> (1, 0)
  U -> (0, 1)
  D -> (0, -1)

data Dist = Dist Vec Int
  deriving Show

type Path = [Dist]

instance Read Dist where
  readsPrec _ s = do
    (d, s) <- reads s
    (n, s) <- reads s
    pure (Dist (dirToVec d) n, s)

parseLine :: String -> Path
parseLine s = read ('[' : s ++ "]")

parseFile :: String -> [Path]
parseFile = fmap parseLine . lines

expandPath :: Vec -> Path -> [Vec]
expandPath o [] = []
expandPath o (Dist _ 0 : ds) = expandPath o ds
expandPath o (Dist v k : ds) =
  o : expandPath (o +++ v) (Dist v (k-1) : ds)

-- | Marks a point in a grid as having a given wire in it.
mark :: Wire -> Vec -> Int -> Grid -> Grid
mark w v n g = alter (pure . fromMaybe n) v w g

markAllPaths :: [(Wire, Path)] -> Grid -> Grid
markAllPaths wps g = foldr f g wps where
  f :: (Wire, Path) -> Grid -> Grid
  f (w, p) g = snd $ foldl' (\(n, g) v -> (n+1, mark w v n g)) (0, g) (expandPath origin p)

-- | Finds all locations where wires cross.
crossings :: Grid -> [(Vec, Int)]
crossings = catMaybes . map f . M.toList where
  f :: (Vec, M.Map Wire Int) -> Maybe (Vec, Int)
  f (v, m) = do
    guard (M.size m > 1)
    pure (v, sum (M.elems m))

main :: IO ()
main = do
  paths <- parseFile <$> readFile "inputs/p3.txt"
  let wps = zip wires paths
  print $ minimum $ map snd $ crossings $ M.delete origin $ markAllPaths wps M.empty
