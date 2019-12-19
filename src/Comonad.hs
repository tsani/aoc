{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Comonad where

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import Data.Maybe ( fromJust, fromMaybe )
import GHC.Generics

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  (<<=) :: w a -> (w a -> b) -> w b

data Z a = Z [a] a [a]
  deriving
    ( Functor
    , Show
    , Read
    , Generic
    , Eq
    , Traversable
    , Foldable
    )

infinite :: a -> Z a
infinite x = Z (repeat x) x (repeat x)

instance Applicative Z where
  pure x = Z [] x []
  Z ll lx lr <*> Z rl rx rr = Z (ll <*> rl) (lx rx) (lr <*> rr) 

instance Comonad Z where
  extract (Z _ x _) = x
  duplicate z = Z (go left z) z (go right z) where
    go next z = case next z of
      Nothing -> []
      Just z -> z : go next z
  (<<=) w f = f <$> duplicate w

(=>>) :: Comonad w => (w a -> b) -> w a -> w b
(=>>) = flip (<<=)

-- | Creates a zipper from a list, putting most of the stuff on the
-- left.
fromListL :: NonEmpty a -> Z a
fromListL (x :| xs) = Z xs x []

fromListR :: NonEmpty a -> Z a
fromListR (x :| xs) = Z [] x xs

dropLeft :: Z a -> NonEmpty a
dropLeft (Z _ x xs) = x :| xs

dropRight :: Z a -> NonEmpty a
dropRight (Z xs x _) = x :| xs

left :: Z a -> Maybe (Z a)
left (Z [] x rs) = Nothing
left (Z (l : ls) x rs) = Just $ Z ls l (x : rs)

right :: Z a -> Maybe (Z a)
right (Z ls x []) = Nothing
right (Z ls x (r : rs)) = Just $ Z (x : ls) r rs

left' :: Z a -> Z a
left' = fromMaybe (error "left': empty list") . left

right' :: Z a -> Z a
right' = fromMaybe (error "right': empty list") . right

most :: (Z a -> Maybe (Z a)) -> Z a -> Z a
most next z = maybe z (most next) $ next z

leftmost :: Z a -> Z a
leftmost = most left

rightmost :: Z a -> Z a
rightmost = most right

toList :: Z a -> NonEmpty a
toList (Z ls x rs) = N.fromList $ ls ++ x : rs
