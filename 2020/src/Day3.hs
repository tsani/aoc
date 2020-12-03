{-# LANGUAGE NamedFieldPuns #-}

module Day3 where

data Tile = Tree | Empty deriving Eq
type Line = [Tile]
type Map = [Line]

data Slope = Slope { right :: Int, down :: Int }

p1 :: IO ()
p1 = print =<< problem Slope { right=3, down=1 } . parse <$> readFile "input/day3.txt"

p2 :: IO ()
p2 = do
  m <- parse <$> readFile "input/day3.txt"
  print $ product $ map (`problem` m) [ Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2 ]

problem :: Slope -> Map -> Int
problem s = length . filter (Tree ==) . path s

-- | Sequence of tiles hit by following the slope in the map.
path :: Slope -> Map -> [Tile]
path _ [] = []
path s@(Slope {right, down}) m@((t : _) : _) = t : path s map' where
  map' = map (drop right) (drop down m)
path _ _ = error "impossible path"

-- Each row is made infinitely long by repeating
parse :: String -> Map
parse = map (cycle . map char) . lines where
  char '#' = Tree
  char '.' = Empty
  char _ = error "impossible char"
