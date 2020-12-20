{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Day12 where

import Data.Bifunctor ( bimap, second )
import Data.List ( foldl' )

data Action = Forward Int | Move Cardinal | Rotate Rotation

type Cardinal = (Int, Int)
type Position = (Int, Int)

(<+>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

both f = bimap f f

type Translation = Position -> Position

north, south, east, west :: Int -> Cardinal
north n = (0, n)
south n = (0, -n)
east n = (n, 0)
west n = (-n, 0)

type Rotation = Cardinal -> Cardinal

step1 :: Cardinal -> Action -> (Cardinal, Translation)
step1 c = \case
  Forward n -> (c, \x -> iterate (c <+>) x !! n)
  Move c' -> (c, \x -> c' <+> x)
  Rotate r -> (r c, id)

step2 :: Cardinal -> Action -> (Cardinal, Translation)
step2 c = \case
  Forward n -> (c, \x -> iterate (c <+>) x !! n)
  Move c' -> (c <+> c', id)
  Rotate r -> (r c, id)

left, right :: Rotation
left (x, y) = (-y, x)
right (x, y) = (y, -x)

common :: ([Action] -> Position -> Position) -> IO ()
common go =
  print . manhattan . (`go` (0, 0)) . parse =<< readFile "input/day12.txt"
  where manhattan = uncurry (+) . both abs

p1 :: IO ()
p1 = common $ snd . foldl' (\(c, t) a -> second (. t) $ step1 c a) (east 1, id)

p2 :: IO ()
p2 = common $ snd . foldl' (\(c, t) a -> second (. t) $ step2 c a) (east 10 <+> north 1, id)

parse :: String -> [Action]
parse = map parseLine . lines where
  parseLine :: String -> Action
  parseLine (c : (read -> n)) = case c of
    'F' -> Forward n
    'N' -> Move $ north n
    'S' -> Move $ south n
    'E' -> Move $ east n
    'W' -> Move $ west n
    'R' -> Rotate (foldr (.) id (replicate (n `div` 90) right))
    'L' -> Rotate (foldr (.) id (replicate (n `div` 90) left))
    _ -> error "impossible input"
