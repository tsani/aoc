{-# LANGUAGE LambdaCase #-}

module Day18 where

import Control.Applicative
import Control.Monad ( MonadPlus )
import Data.List ( foldl' )
import Data.Maybe ( fromJust )

import Parser

data Exp = Lit !Int | Bin !Op !Exp !Exp deriving Show

data Op = Add | Mul deriving Show

p1 :: IO ()
p1 = print . sum . map eval . parse parseExp =<< readFile "input/day18.txt"

p2 :: IO ()
p2 = print . sum . map eval . parse parseExp' =<< readFile "input/day18.txt"

parse :: Parser Maybe Exp -> String -> [Exp]
parse p = map (snd . fromJust . runParser (only p)) . lines

op :: Op -> Int -> Int -> Int
op = \case
  Add -> (+)
  Mul -> (*)

eval :: Exp -> Int
eval (Lit n) = n
eval (Bin o e1 e2) = op o (eval e1) (eval e2)

parseExp :: MonadPlus m => Parser m Exp
parseExp = parseBin <|> parseLit where
  parseBin =
    foldl' (\l (o, r) -> Bin o l r) <$> parseLit <*> many ((,) <$> parseOp <*> parseLit)

parseOp, add, mul :: MonadPlus m => Parser m Op
parseOp = add <|> mul
add = lexeme (char '+') *> pure Add
mul = lexeme (char '*') *> pure Mul

parseLit :: MonadPlus m => Parser m Exp
parseLit = lexeme (Lit <$> uint)

parseExp' :: MonadPlus m => Parser m Exp
parseExp' = parseMul where
  parseMul = (Bin Mul <$> parseAdd <* lexeme (char '*') <*> parseMul) <|> parseAdd
  parseAdd = (Bin Add <$> parseBot <* lexeme (char '+') <*> parseAdd) <|> parseBot
  parseBot = parseLit <|> parenthesized parseExp'

test :: String -> Int
test = eval . head . parse parseExp'
