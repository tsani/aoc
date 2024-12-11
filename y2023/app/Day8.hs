{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day8 where

import Control.Applicative
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe ( fromJust )

import Parser

data Dir = L | R
  deriving (Eq, Ord, Show, Read)

dir L = fst
dir R = snd

type Label = String
type Map = M.Map Label (Label, Label)

data Input = Input { directions :: [Dir], nodes :: Map }
  deriving (Show)

parse :: String -> Input
parse = snd . fromJust . runParser wholeInput where
  wholeInput = do
    directions <- map (read . pure) <$> many (satisfy (`elem` "LR"))
    newline
    newline
    nodes <- M.fromList <$> (row `sepBy` newline)
    pure $ Input { directions, nodes }

  row = do
    start <- exactly 3 anyChar
    string " = ("
    left <- exactly 3 anyChar
    string ", "
    right <- exactly 3 anyChar
    string ")"
    pure $ (start, (left, right))

initial :: Label -> Bool
initial (_:_:'A':[]) = True
initial _ = False

terminal :: Label -> Bool
terminal (_:_:'Z':[]) = True
terminal _ = False

search :: Int -> [Dir] -> Map -> Label -> Int
search !dist (d:ds) m here
  | terminal here = dist
  | otherwise = search (dist+1) ds m $ dir d (fromJust (M.lookup here m))

go :: Input -> Int
go Input { directions, nodes } = search 0 (cycle directions) nodes "AAA"

-- go2 :: Input -> Int
go2 Input { directions, nodes } = foldr lcm 1 distances where
  initials = filter initial $ map fst $ M.toList nodes
  distances = map (search 0 (cycle directions) nodes) initials

main :: IO ()
main = print =<< go2 . parse <$> readFile "input/day8.txt"
