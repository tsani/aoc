{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module P12 where

import Comonad
import Parser

import qualified Data.List.NonEmpty as N
import Data.Maybe ( fromJust, fromMaybe )

data VF a = V { _x :: !a, _y :: !a, _z :: !a }
  deriving (Show, Foldable, Functor, Traversable) -- so I can use fmap later :)

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
  deriving Show

energy :: Obj -> Int
energy Obj { .. } = pot * kin where
  e k = sum $ map (abs . ($ k)) [_x, _y, _z]
  pot = e pos
  kin = e vel

static :: V -> Obj
static pos = Obj { vel = zero, .. }

-- | Initial system. (Puzzle input.)
initial =
  [ static $ V 1 3 (-11)
  , static $ V 17 10 (-8)
  , static $ V (-1) (-15) 2
  , static $ V 12 (-4) (-4)
  ]

-- | attract o1 o2 updates the velocity of o1 according to its
-- attraction by o2.
attract :: Obj -> Obj -> Obj
Obj { vel=v1, pos=p1 } `attract` Obj { vel=v2, pos=p2 } =
  Obj { vel=v1', pos=p1 } where
    v1' = v1 +! collapse (p1 -! p2) where
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
  fromListR . N.fromList . map (static . parseV) . lines <$> readFile "inputs/p12-test1.txt"

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
