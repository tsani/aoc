{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Z where

import Control.DeepSeq
import Data.Maybe ( fromJust )
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import GHC.Generics

import Comonad
import Util ( iterateMaybe )

-- | A zipper for @a@.
data Z a = Z [a] a [a]
  deriving
    ( Eq
    , Foldable
    , Functor
    , Generic
    , NFData
    , Ord
    , Show
    , Traversable
    )

instance Comonad Z where
  duplicate z = Z (tail $ lefts z) z (tail $ rights z)
  extract (Z _ x _) = x

-- | Moves left as much as possible.
seekLeft :: Z a -> Z a
seekLeft z = maybe z seekLeft (left z)

-- | Moves right as much as possible.
seekRight :: Z a -> Z a
seekRight z = maybe z seekRight (right z)

-- | All the times you can go left, including zero times!
lefts :: Z a -> [Z a]
lefts = iterateMaybe left

-- | All the times you can go right, including zero times!
rights :: Z a -> [Z a]
rights = iterateMaybe right

left :: Z a -> Maybe (Z a)
left (Z [] _ _) = Nothing
left (Z (l:ls) x rs) = Just $ Z ls l (x:rs)

left' :: Z a -> Z a
left' = fromJust . left

right :: Z a -> Maybe (Z a)
right (Z _ _ []) = Nothing
right (Z ls x (r:rs)) = Just $ Z (x:ls) r rs

right' :: Z a -> Z a
right' = fromJust . right

leftN :: Int -> Z a -> Maybe (Z a)
leftN 0 = Just
leftN n = (leftN (n-1) =<<) . left

leftN' :: Int -> Z a -> Z a
leftN' n z = iterate left' z !! n

rightN' :: Int -> Z a -> Z a
rightN' n z = iterate right' z !! n

rightN :: Int -> Z a -> Maybe (Z a)
rightN 0 = Just
rightN n = (rightN (n-1) =<<) . right

fromRight :: NonEmpty a -> Z a
fromRight (x :| rs) = Z [] x rs

fromLeft :: NonEmpty a -> Z a
fromLeft (x :| ls) = Z ls x []

toRight :: Z a -> NonEmpty a
toRight (Z _ x rs) = x :| rs

toLeft :: Z a -> NonEmpty a
toLeft (Z ls x _) = x :| ls

toList :: Z a -> NonEmpty a
toList = toRight . seekLeft

moveN :: Int -> Z a -> Maybe (Z a)
moveN n
  | n >= 0 = rightN n
  | n < 0 = leftN (-n)
  | otherwise = error "impossible integer"

-- | Moves left or right according to the sign of the integer.
moveN' :: Int -> Z a -> Z a
moveN' n
  | n >= 0 = rightN' n
  | n < 0 = leftN' (-n)
  | otherwise = error "impossible integer"

modify :: (a -> a) -> Z a -> Z a
modify f (Z ls x rs) = Z ls (f x) rs

nice :: (a -> Char) -> Z a -> String
nice f (Z ls x rs) = (f <$> reverse ls) ++ '|' : f x : '|' : (f <$> rs)

length :: Z a -> Int
length (Z ls x rs) = Prelude.length ls + 1 + Prelude.length rs
