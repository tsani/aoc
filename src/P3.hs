module P3 where

import Data.List ( minimumBy )
import Data.Coerce
import Data.Maybe (fromMaybe)
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

type Grid = M.Map Vec (S.Set Wire)

glookup :: Vec -> Grid -> S.Set Wire
glookup k g = fromMaybe mempty $ M.lookup k g

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
mark :: Wire -> Vec -> Grid -> Grid
mark w v g = M.alter (pure . S.insert w . fromMaybe mempty) v g

markAllPaths :: [(Wire, Path)] -> Grid -> Grid
markAllPaths wps g = foldr f g wps where
  f :: (Wire, Path) -> Grid -> Grid
  f (w, p) g = foldr (mark w) g (expandPath origin p)

-- | Finds all locations where wires cross.
crossings :: Grid -> [Vec]
crossings = map fst . filter f . M.toList where
  f :: (Vec, S.Set Wire) -> Bool
  f = (> 1) . S.size . snd

manhattan :: Vec -> Int
manhattan (x, y) = abs x + abs y

main :: IO ()
main = do
  paths <- parseFile <$> readFile "inputs/p3.txt"
  print $ minimum $ map manhattan $ crossings $ M.delete origin $ markAllPaths (zip wires paths) M.empty
