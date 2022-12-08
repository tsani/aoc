module Day6 (main) where

import Data.List ( nub )
import System.IO

groupBySize :: Eq a => Int -> [a] -> [[a]]
groupBySize _ [] = []
groupBySize n l = take n l : groupBySize n (tail l)

isStart :: Eq a => Int -> [a] -> Bool
isStart k = (k ==) . length . nub

answer :: Int -> String -> Int
answer k = (k +) . fst . head . filter (isStart k . snd) . zip [0..] . groupBySize k

main :: IO ()
main = withFile "input/day6.txt" ReadMode $ \h ->
  print =<< answer 14 <$> hGetContents h
