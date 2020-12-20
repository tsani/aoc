{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day13 where

import Data.Bifunctor ( second )
import Data.Ord ( comparing )
import Data.List ( maximumBy, minimumBy )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding ( gcd )

data Problem = Problem { earliest :: Integer, buses :: [(Integer, Integer)] }

dup :: a -> (a, a)
dup x = (x, x)

readText :: Read a => Text -> a
readText = read . T.unpack

parse :: Text -> Problem
parse (T.lines -> [l1, l2]) = Problem
  { earliest = readText l1
  , buses = map (second readText) . filter (("x" /=) . snd) . zip [0..] . T.splitOn "," $ l2
  }
parse _ = error "bogus input"

common k = print . k =<< readProblem

p1 :: IO ()
p1 = common solve1

p2 :: IO ()
p2 = common (solve2' . buses)

readProblem :: IO Problem
readProblem = parse <$> T.readFile "input/day13.txt"

solve1 :: Problem -> Integer
solve1 Problem { earliest, buses } = f buses where
  f =
    uncurry (*) .
    second (subtract earliest) .
    minimumBy (comparing snd) .
    map (second (next earliest) . dup . snd)

next :: Integer -> Integer -> Integer
next earliest b
  | r == 0 = earliest
  | otherwise = (q + 1) * b
  where
  (q, r) = earliest `divMod` b

check :: [(Integer, Integer)] -> Integer -> Bool
check buses t = all (\(i, k) -> i == next t k - t) buses

solve2 :: Integer -> [(Integer, Integer)] -> Integer
solve2 start buses = head $ filter (check buses) (iterate (bestInc +) bestStart) where
  (d, bestInc) = maximumBy (comparing snd) buses
  bestStart = next start bestInc - d

-- Brute force is too slow... We need to use the Chinese Remainder Theorem.

solve2' :: [(Integer, Integer)] -> (Integer, Integer)
solve2' = crt . setup

setup = map (\(i, k) -> ((k - i) `mod` k, k))

-- | The extended Euclidean algorithm.
-- if egcd a b = (r, s, t), then
--   r = gcd(a, b) = s*a + t*b
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a b = go (a, b) (1, 0) (0, 1) where
  go (r0, r1) (s0, s1) (t0, t1) = case r0 `divMod` r1 of
    (_, 0) -> (r1, s1, t1)
    (q, r) -> go (r1, r) (s1, s0 - q*s1) (t1, t0 - q*t1)


-- | The Chinese Remainder Theorem for 2 moduli.
-- If n1 and n2 are coprime and (a, n1 * n2) = crt2 (a1, n1) (a2, n2),
-- then a = a1 (mod n1)
--      a = a2 (mod n2)
crt2 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
crt2 (a1, n1) (a2, n2) = case egcd n1 n2 of
  (1, m1, m2) -> ((a1 * n2 * m2 + a2 * n1 * m1) `mod` (n1 * n2), n1 * n2)
  (_, _, _) -> error "egcd: n1 n2 not coprime"

-- | The Chinese Remainder Theorem for a system with at least 1 equation.
crt :: [(Integer, Integer)] -> (Integer, Integer)
crt [] = error "impossible to take CRT of zero equations"
crt es = foldr1 crt2 es
