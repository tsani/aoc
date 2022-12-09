{-# LANGUAGE LambdaCase, NamedFieldPuns, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Day9 (main) where

import Data.List ( scanl' )
import qualified Data.Set as S

data Move = U | D | L | R
  deriving Read

type Program = [Move]

parse :: String -> Program
parse = concatMap (f . words) . lines where
  f [d, n] = replicate (read n) (read d) where

data Pair a = Pair !a !a
  deriving (Functor, Foldable, Traversable, Show, Ord, Eq)

type V2 = Pair Int

(+++) :: V2 -> V2 -> V2
Pair x1 y1 +++ Pair x2 y2 = Pair (x1 + x2) (y1 + y2)
infixl 4 +++

(***) :: Int -> V2 -> V2
k *** Pair x y = Pair (k * x) (k * y)
infixl 5 ***

-- | Subtracts the second vector from the first vector.
diff :: V2 -> V2 -> V2
diff v1 v2 = v1 +++ ((-1) *** v2)

move :: Move -> V2
move = \case
  U -> Pair 0 1
  D -> Pair 0 (-1)
  L -> Pair (-1) 0
  R -> Pair 1 0

data State = State { hd :: V2, tl :: [V2] }

interpret :: State -> Move -> State
interpret s m = s' where
  s' = State { hd = hd s +++ move m, tl = fixTail (hd s') (tl s) }

  fixTail _ [] = []
  fixTail hd (tl1 : tl) = tl1' : fixTail tl1' tl where
    tl1' = let x = tl1 +++ (signum <$> diff hd tl1) in
      if x == hd then tl1 else x

initialState :: Int -> State
initialState n = State { hd = Pair 0 0, tl = replicate n (Pair 0 0) }

run :: State -> Program -> [State]
run = scanl' interpret

answer :: Int -> String -> Int
answer n = S.size . S.fromList . map (last . tl) . run (initialState n) . parse

answer1, answer2 :: String -> Int
answer1 = answer 1
answer2 = answer 9

main :: IO ()
main = print =<< answer2 <$> readFile "input/day9.txt"
