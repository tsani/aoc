{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S

p2 :: IO ()
p2 = print =<< go <$> T.readFile "input/day6.txt" where
  go = sum . fmap (S.size . foldr S.intersection u . map (S.fromList . T.unpack) . T.splitOn "\n") . T.splitOn "\n\n"
  u = S.fromList ['a'..'z']
