module Day3 (main) where

import qualified Data.Set as S
import System.IO

halfCut :: [a] -> ([a], [a])
halfCut l = splitAt (length l `div` 2) l

type Sack = (String, String)

groupByLength :: Int -> [a] -> [[a]]
groupByLength _ [] = []
groupByLength n l =
  let (x, l') = splitAt n l in
  x : groupByLength n l'

priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = fromEnum c - fromEnum 'a' + 1
  | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 27
  | otherwise = error "bad input"

findCommonItem :: Sack -> Char
findCommonItem (com1, com2) = head . S.toList $ S.fromList com1 `S.intersection` S.fromList com2

answer1 = sum . map (priority . findCommonItem . halfCut) . lines

answer2 = sum . map (priority . head . S.toList . foldr1 S.intersection . map S.fromList) . groupByLength 3 . lines

main :: IO ()
main = withFile "input/day3.txt" ReadMode $ \h ->
  print =<< answer2 <$> hGetContents h
