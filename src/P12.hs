{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module P12 where

import Comonad
import Parser

import Control.Arrow
import Data.Bifunctor ( bimap )
import Data.Function ( on )
import qualified Data.List.NonEmpty as N
import Data.Maybe ( fromJust, fromMaybe )

data VF a = V { _x :: !a, _y :: !a, _z :: !a }
  deriving (Eq, Show, Foldable, Functor, Traversable) -- so I can use fmap later :)

type V = VF Int

k *! V x y z = V (k * x) (k * y) (k * z)

(-!) :: V -> V -> V
v1 -! v2  = v1 +! ((-1) *! v2)

V x1 y1 z1 +! V x2 y2 z2 = V (x1 + x2) (y1 + y2) (z1 + z2)

zero = V 0 0 0

data Obj =
  Obj
  { pos :: V
  , vel :: V
  }
  deriving (Eq, Show)

energy :: Obj -> Int
energy Obj { .. } = pot * kin where
  e k = sum $ map (abs . ($ k)) [_x, _y, _z]
  pot = e pos
  kin = e vel

totalEnergy :: System -> Int
totalEnergy = sum . fmap energy . toList

static :: V -> Obj
static pos = Obj { vel = zero, .. }

-- | attract o1 o2 updates the velocity of o1 according to its
-- attraction by o2.
attract :: Obj -> Obj -> Obj
Obj { vel=v1, pos=p1 } `attract` Obj { vel=v2, pos=p2 } =
  Obj { vel=v1', pos=p1 } where
    v1' = v1 +! collapse (p2 -! p1) where
      collapse = fmap signum

type System = Z Obj

zAttract :: Z Obj -> Obj
zAttract (Z ls x rs) = foldr (flip attract) x (ls ++ rs)

-- | Executes one time-step of the simulation.
step :: System -> System
step = fmap applyVelocity . (=>>) zAttract where
  applyVelocity :: Obj -> Obj
  applyVelocity (Obj {..}) = Obj { vel, pos = vel +! pos }

loadSystem :: IO System
loadSystem =
  fromListR . N.fromList . map (static . parseV) . lines <$> readFile "inputs/p12.txt"

parseV :: String -> V
parseV s = fromMaybe (error "parse failed") $ parse s $ do
  string "<x="
  x <- integer
  string ", y="
  y <- integer
  string ", z="
  z <- integer
  char '>'
  eof
  pure $ V x y z

answer1 :: System -> Int
answer1 = totalEnergy . (!! 1000) . iterate step

-- | Finds all pairs of indices at which the raced entities become the same.
-- The first result is always (0, 0) since they start identically.
raceBy :: forall a. (a -> a -> Bool) -> (a -> a) -> a -> [(Int, Int)]
raceBy p f z = map (fst *** fst) $ filter (\((i1, s1), (i2, s2)) -> p s1 s2) $ uncurry zip seqs where
  seqs = zip [0..] `both` (iterate f z, iterate (f . f) z) where
    both f = bimap f f

-- answer2 :: System -> (Int, Int, Int)
answer2 s = (x, y, z) where
  raceCoord sel = {- (uncurry $ flip (-)) . -} (!! 1) . raceBy p step where
    -- checks if two _systems_ are equal on all objects, for the given coordinate
    p s1 s2 = all (uncurry pObj) $ N.zip (toList s1) (toList s2)
    -- checks if two objects are equal on a given coordinate for position & velocity
    pObj o1 o2 = sel (pos o1) == sel (pos o2) && sel (vel o1) == sel (vel o2)

  -- race each coordinate separately
  x = raceCoord _x s
  y = raceCoord _y s
  z = raceCoord _z s

  -- calculate the lcm for each coordinate
  -- p = lcm x (lcm y z)

main2 :: IO ()
main2 = do
  print . take 2 . raceBy (==) step =<< loadSystem
