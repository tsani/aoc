module Util where

import qualified Data.Text as T

type Range = (Int, Int)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : case f x of
  Just x' -> iterateMaybe f x'
  Nothing -> []

iterateMaybe1 :: (a -> Maybe a) -> a -> [a]
iterateMaybe1 f = tail . iterateMaybe f

readText :: Read a => T.Text -> a
readText = read . T.unpack

inRange :: Int -> Range -> Bool
inRange n (lo, hi) = lo <= n && n <= hi
