{-# LANGUAGE ViewPatterns #-}

module P10 where

-- Idea: sort all asteroids by (manhattan) distance to the desired point.
-- For each asteroid, calculate the simplified displacement fraction r.
-- Delete any asteroid *later* in the list if its position is k*r for
-- some natural number k.

import Control.Applicative ( (<|>) )
import Control.Monad ( guard )
import Data.Function ( on )
import Data.List ( sortBy, maximumBy, groupBy )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import Data.Ratio
import Data.Tuple ( swap )
import qualified Data.Set as S

type V = (Int, Int)
type Map = [V]

distanceTo :: V -> V -> Int
distanceTo (d1, d2) (x1, x2) = abs (d1 - x1) + abs (d2 - x2)

-- | v1 -! v2 calculates the displacement from v2 to v1.
(-!) :: V -> V -> V
(x1, x2) -! (y1, y2) = (x1 - y1, x2 - y2)

(+!) :: V -> V -> V
(x1, x2) +! (y1, y2) = (x1 + y1, x2 + y2)

-- | Computes the list of all asteroids visible from a given origin
-- (which is assumed to be an asteroid).
visible :: V -> Map -> [V]
visible o (sortBy (comparing (distanceTo o)) -> m) = go (tail m) where
  -- we take the tail because the head will be o and we don't want to
  -- consider it
  go [] = []
  go (v : vs) = v : go (removeBlocked vs) where
    removeBlocked = filter (not . \v' -> simplify (v -! o) `blocks` (v' -! o))

-- | Given a displacement vector, simplifies it.
simplify :: V -> V
simplify (n, 0) = (abs n `div` n, 0)
simplify (0, k) = (0, abs k `div` k)
simplify (n, k) = (signum n * n', signum k * k') where
  r = abs n % abs k
  (n', k') = (numerator r, denominator r)

-- | `blocks (d1, d2) (d1', d2')` given two displacements from an
-- origin decides whether the asteroid at the first blocks the second.
blocks :: V -> V -> Bool
blocks (d1, d2) (d1', d2') = maybe False (const True) go where
  go = do
    k <- (d1' `div` d1) <|> (d2' `div` d2)
    guard (k > 0 && k * d1 == d1' && k * d2 == d2')

  div _ 0 = Nothing
  div n d = case n `quotRem` d of
    (k, 0) -> Just k
    _ -> Nothing

-- | Loads the coordinates of all the asteroids.
load :: IO [V]
load = do
  let reshape (y, (x, c)) = ((x, y), c)
  map fst . filter ((== '#') . snd) . concatMap (map reshape . sequence) . zip [0..] . map (zip [0..]) . lines
    <$> readFile path

(station, path) =
  ((37, 25), "inputs/p10.txt")
  -- ((8, 3), "inputs/p10-2-test1.txt")
  -- ((11, 13), "inputs/p10-test-big.txt")

-- | Variant of atan2 that returns 0 for straight up and increasing for clockwise from there.
atan2' :: Double -> Double -> Double
atan2' y x =
  let t = pi/2 - atan2 y x in
  if t < 0 then
    t + 2 * pi
  else
    t

-- | `angleTo p0 p` calculates the angle from `p0` to `p`.
angleTo :: V -> V -> Double
angleTo p0 p = atan2' (fromIntegral (- dy)) (fromIntegral dx) where
  (dx, dy) = p -! p0

-- | Approximate equality for doubles.
(~=) :: Double -> Double -> Bool
d1 ~= d2 = abs (d1 - d2) < 0.00001

shave :: Int -> [[V]] -> V
shave n vs
  | n < length vs = head (vs !! n)
  | otherwise = shave (n - length vs) (filter (not . null) $ map tail vs)

angleTest = map ((/pi) . angleTo station) l where
  l = map (station +!) [ (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1) ]
  
answer2 :: IO ()
answer2 = do
  m <- load
  putStr "all asteroids: "
  print m
  let m' = groupBy ((~=) `on` angleTo station)
           $ sortBy (comparing (angleTo station) `mappend` comparing (distanceTo station))
           $ m
  putStr "sorted: "
  print m'
  -- print (map (\x -> (angleTo station x, x)) m)
  -- let (x, y) = shave 0 m
  let (x, y) = shave 199 m'
  print $ (x, y)
  print (100 * x + y)

test :: IO (Int -> V)
test = do
  m <- load
  let m' = groupBy ((~=) `on` angleTo station)
           $ sortBy (comparing (angleTo station) `mappend` comparing (distanceTo station))
           $ m
  pure $ \n ->
    shave n m'
    

answer :: IO [V]
answer = do
  m <- load
  -- m is a list of all coordinates where there is an asteroid
  print $ maximumBy (comparing snd) $ map (\a -> (a, length $ visible a m)) m
  pure []
  -- pure $ sortBy (comparing (distanceTo (5, 8))) $ visible (5, 8) m

main :: IO ()
main = print . length =<< answer
