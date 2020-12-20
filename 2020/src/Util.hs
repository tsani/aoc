module Util where

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : case f x of
  Just x' -> iterateMaybe f x'
  Nothing -> []

iterateMaybe1 :: (a -> Maybe a) -> a -> [a]
iterateMaybe1 f = tail . iterateMaybe f
