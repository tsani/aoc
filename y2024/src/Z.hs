{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}

module Z
( Z(..)
, left
, right
, lefts
, rights
, fromList
, toList
, module Comonad
) where

import Control.Applicative

import Comonad

data Z a = Z { leftSide :: [a], center :: a, rightSide :: [a] }
  deriving (Functor, Eq, Ord, Show)

type Motion a = Z a -> Maybe (Z a)

left :: Motion a
left (Z [] _ _) = Nothing
left (Z (l:ls) x rs) = Just $ Z ls l (x:rs)

right :: Motion a
right (Z _ _ []) = Nothing
right (Z ls x (r:rs)) = Just $ Z (x:ls) r rs

-- | Take the center and everything on the left. Never empty.
lefts :: Z a -> [a]
lefts (Z l x _) = x:l

-- | Take the center and everything on the right. Never empty.
rights :: Z a -> [a]
rights (Z _ x r) = x:r

instance Applicative Z where
  pure x = Z (repeat x) x (repeat x)
  Z fls f frs <*> Z xls x xrs = Z ls y rs where
    ls = getZipList $ ZipList fls <*> ZipList xls
    y = f x
    rs = getZipList $ ZipList frs <*> ZipList xrs

instance Comonad Z where
  extract (Z _ x _) = x
  duplicate z = center where
    center = Z leftSide z rightSide

    leftSide = case duplicate <$> left z of
      Nothing -> []
      Just zl -> lefts $ zl { rightSide = rights center }

    rightSide = case duplicate <$> right z of
      Nothing -> []
      Just zr -> rights $ zr { leftSide = lefts center }

fromList :: [a] -> Maybe (Z a)
fromList [] = Nothing
fromList (x:xs) = Just $ Z [] x xs

toList :: Z a -> [a]
toList (Z ls x rs) = ls ++ [x] ++ rs

iterateMaybe :: (a -> Maybe a) -> a -> a
iterateMaybe f x = maybe x (iterateMaybe f) (f x)

allTheWay :: Motion a -> Z a -> Z a
allTheWay = iterateMaybe
