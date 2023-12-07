{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Parser
  ( Parser(..)
  , satisfy
  , char
  , string
  , sepBy
  , number
  , spaces
  , newline
  , letters
  ) where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser $ \s -> pure (s, x)
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ pure Nothing
  Parser f1 <|> Parser f2 = Parser $ \s -> f1 s <|> f2 s

instance Monad Parser where
  Parser f >>= k = Parser $ \s -> do
    (s', x) <- f s
    runParser (k x) s'

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> Nothing
  x : xs -> if p x then Just (xs, x) else Nothing

char :: Char -> Parser ()
char c = void $ satisfy (c ==)

string :: String -> Parser ()
string [] = pure ()
string (c : cs) = char c *> string cs

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

number :: Parser Int
number = read <$> some (satisfy isDigit)

spaces :: Parser ()
spaces = void $ some (string " ")

newline :: Parser ()
newline = string "\n"

letters :: Parser String
letters = some (satisfy isLetter)
