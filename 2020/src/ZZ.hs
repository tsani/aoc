{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Two-dimensional zippers.
module ZZ where

import Data.List.NonEmpty ( NonEmpty )
import Data.Maybe ( fromJust )

import Z (Z(..))
import qualified Z
import Comonad
import Util ( iterateMaybe1 )

-- | Two dimensional zipper for @a@.
newtype ZZ a = ZZ { unZZ :: Z (Z a) }
  deriving (Eq, Ord, Functor, Show, Foldable, Traversable)

fromList :: NonEmpty (NonEmpty a) -> ZZ a
fromList = ZZ . Z.fromRight . fmap Z.fromRight

toList :: ZZ a -> NonEmpty (NonEmpty a)
toList = Z.toRight . fmap Z.toRight . unZZ

instance Comonad ZZ where
  duplicate (ZZ zz) = fmap ZZ $ ZZ $ roll $ roll zz
  extract (ZZ zz) = extract (extract zz)

roll :: Z (Z a) -> Z (Z (Z a))
roll a = Z (go Z.left a) a (go Z.right a) where
  go d = iterateMaybe1 (traverse d)

seekLeft :: ZZ a -> ZZ a
seekLeft z = maybe z seekLeft (left z)

left :: ZZ a -> Maybe (ZZ a)
left = fmap ZZ . sequenceA . (fmap Z.left) . unZZ

left' :: ZZ a -> ZZ a
left' = fromJust . left

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

down :: ZZ a -> Maybe (ZZ a)
down = fmap ZZ . Z.right . unZZ

down' :: ZZ a -> ZZ a
down' = fromJust . down
