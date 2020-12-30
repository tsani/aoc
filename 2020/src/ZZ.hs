{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Two-dimensional zippers.
module ZZ where

import Control.DeepSeq
import Data.List.NonEmpty ( NonEmpty )
import Data.Maybe ( fromJust )
import GHC.Generics

import Z (Z(..))
import qualified Z
import Comonad
import Util ( iterateMaybe1 )

-- | Two dimensional zipper for @a@.
newtype ZZ a = ZZ { unZZ :: Z (Z a) }
  deriving (Eq, Ord, Functor, Show, Foldable, Traversable)
  deriving newtype NFData

fromList :: NonEmpty (NonEmpty a) -> ZZ a
fromList = ZZ . Z.fromRight . fmap Z.fromRight

toList :: ZZ a -> NonEmpty (NonEmpty a)
toList = Z.toRight . fmap Z.toRight . unZZ

instance Comonad ZZ where
  duplicate (ZZ zz) = fmap ZZ $ ZZ $ roll $ roll zz
  extract (ZZ zz) = extract (extract zz)

-- | Applies a transformation to the value under focus.
modify :: (a -> a) -> ZZ a -> ZZ a
modify f (ZZ zz) = ZZ $ Z.modify (Z.modify f) zz

roll :: Z (Z a) -> Z (Z (Z a))
roll a = Z (go Z.left a) a (go Z.right a) where
  go d = iterateMaybe1 (traverse d)

seekLeft :: ZZ a -> ZZ a
seekLeft z = maybe z seekLeft (left z)

left :: ZZ a -> Maybe (ZZ a)
left = fmap ZZ . sequenceA . (fmap Z.left) . unZZ

left' :: ZZ a -> ZZ a
left' = fromJust . left

seekRight :: ZZ a -> ZZ a
seekRight z = maybe z seekRight (right z)

right :: ZZ a -> Maybe (ZZ a)
right = fmap ZZ . sequenceA . (fmap Z.right) . unZZ

right' :: ZZ a -> ZZ a
right' = fromJust . right

seekUp :: ZZ a -> ZZ a
seekUp z = maybe z seekUp (up z)

up :: ZZ a -> Maybe (ZZ a)
up = fmap ZZ . Z.left . unZZ

up' :: ZZ a -> ZZ a
up' = fromJust . up

seekDown :: ZZ a -> ZZ a
seekDown z = maybe z seekDown (down z)

down :: ZZ a -> Maybe (ZZ a)
down = fmap ZZ . Z.right . unZZ

down' :: ZZ a -> ZZ a
down' = fromJust . down

-- | Assuming the grid is rectangular and finite, returns the values
-- in the four corners in clockwise order @(tl, tr, br, bl)@.
corners :: ZZ a -> (a, a, a, a)
corners zz =
  ( v (seekLeft . seekUp)
  , v (seekLeft . seekDown)
  , v (seekRight . seekUp)
  , v (seekRight . seekDown)
  ) where
  v f = extract (f zz)

-- | Equips each point in the grid with its (column, row) position,
-- assuming that the current focus's position is (0, 0).
positions :: ZZ a -> ZZ ((Int, Int), a)
positions (ZZ (Z ls x rs)) =
  ZZ $ Z (zipWith col [-1,-2..] ls) (col 0 x) (zipWith col [1,2..] rs) where
    col i (Z ls x rs) =
      Z (zipWith (row i) [-1,-2..] ls) (row i 0 x) (zipWith (row i) [1,2..] rs)
    row i j x = ((i, j), x)

height :: ZZ a -> Int
height (ZZ zz) = Z.length zz

width :: ZZ a -> Int
width (ZZ zz) = Z.length (extract zz)
