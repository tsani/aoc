{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day8 where

import Data.Bifunctor
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N
import Data.Functor ( ($>) )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Comonad
import Z

data Opcode = Nop | Acc | Jmp

type Program= [Instr]
type Instr = (Opcode, Int)

type State = (Int, Z (Instr, Bool))

p1 :: IO ()
p1 = print =<< go . initialize . parse <$> T.readFile "input/day8.txt" where
  initialize = (0,) . fmap (\i -> (i, False))
  go = run step

p2 :: IO ()
p2 = print =<< go . N.toList . initialize . parse <$> T.readFile "input/day8.txt" where
  initialize = toRight . fmap ((0,) . seekLeft . fmap ((, False)) . variant) . duplicate

  go :: [State] -> [Int]
  go = map snd . filter halts . map (run step) where
    halts (Halt, _) = True
    halts _ = False

  variant = modify (first f) where
    f = \case
      Nop -> Jmp
      Jmp -> Nop
      x -> x

run :: (a -> Either b a) -> a -> b
run f x = either id (run f) (f x)

data Status = Halt | Infinite deriving Show

-- | Executes one step of the CPU.
-- If the CPU halts or enters an infinite loop, gives Left with the
-- status and value of the accumulator.
step :: State -> Either (Status, Int) State
step (acc, z@(Z _ (_, True) _)) = Left (Infinite, acc)
step (acc, z@(Z _ (instr, False) _)) = case instr of
  (Nop, _) -> case rz' of
    Nothing -> Left (Halt, acc)
    Just z'' -> Right (acc, z'')
  (Acc, n) -> case rz' of
    Nothing -> Left (Halt, acc)
    Just z'' -> Right (acc + n, z'')
  (Jmp, n) -> case moveN n z' of
    Nothing -> Left (Halt, acc)
    Just z'' -> Right (acc, z'')
  where
    rz' = right z'
    z' = modify ($> True) z

parse :: T.Text -> Z Instr
parse = fromRight . fmap (parseLine . T.words) . N.fromList . T.lines

parseLine :: [T.Text] -> Instr
parseLine [opcode, offset] = (parseOpcode opcode, parseOffset offset) where
  parseOpcode x = case () of
    _ | x == "jmp" -> Jmp
    _ | x == "nop" -> Nop
    _ | x == "acc" -> Acc
    _ -> error "impossible opcode"

  parseOffset (T.unpack -> s:num) = parseSign s (read num) where
    parseSign '-' = (* (-1))
    parseSign '+' = id
    parseSign _ = error "impossible sign"
