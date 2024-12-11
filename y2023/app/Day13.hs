{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, LambdaCase #-}
module Day13 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List

import Parser

data L
  = I Int
  | L [L]
  deriving Eq

instance Ord L where
  compare (I i1) (I i2) = compare i1 i2
  compare (I i) (L l) = compare (L [I i]) (L l)
  compare (L l) (I i) = compare (L l) (L [I i])
  compare (L []) (L []) = EQ
  compare (L (x1 : xs1)) (L (x2 : xs2)) = compare x1 x2 <> compare (L xs1) (L xs2)
  compare (L []) (L _) = LT
  compare (L _) (L _) = GT

parseLine :: String -> L
parseLine input = snd . fromJust $ runParser parseItem input where
  parseItem :: Parser L
  parseItem = parseNumber <|> parseList

  parseNumber = I <$> number

  parseList = do
    char '['
    ls <- parseItem `sepBy` char ','
    char ']'
    pure $ L ls

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p l = let (x, rest) = break p l in x : case rest of
  [] -> []
  _ : tl -> splitOn p tl

answer1 :: String -> Int
answer1 = sum . map fst . filter (uncurry (<=) . snd) . zip [1..] . parse where
  parse :: String -> [(L, L)]
  parse = map (go . map parseLine) . splitOn null . lines where
    go [l1, l2] = (l1, l2)

answer2 :: String -> Int
answer2 = product . map fst . filter ((`elem` dividers) . snd) . zip [1..] . sort . (dividers ++) . parse where
  parse = map parseLine . filter (not . null) . lines

dividers :: [L]
dividers = [L [L [I 2]], L [L [I 6]]]

main :: IO ()
main = print . answer2 =<< readFile "input/day13.txt"
