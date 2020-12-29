{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser m a = Parser { runParser :: String -> m (String, a) }
  deriving Functor

instance Monad m => Applicative (Parser m) where
  pure x = Parser $ \s -> pure (s, x)
  Parser f <*> Parser a = Parser $ \s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    pure (s'', f' a')

instance Monad m => Monad (Parser m) where
  return = pure
  Parser a >>= k = Parser $ \s -> do
    (s', a') <- a s
    k a' `runParser` s'

instance MonadPlus m => Alternative (Parser m) where
  empty = Parser (const empty)
  Parser a <|> Parser b = Parser $ \s -> a s <|> b s

only :: MonadPlus m => Parser m a -> Parser m a
only = (<* eof)

eof :: MonadPlus m => Parser m ()
eof = Parser $ \case
  [] -> pure ([], ())
  _ -> empty

satisfy :: MonadPlus m => (Char -> Bool) -> Parser m Char
satisfy p = Parser $ \case
  c : s | p c -> pure (s, c)
  _ -> empty

char :: MonadPlus m => Char -> Parser m Char
char = satisfy . (==)

string :: MonadPlus m => String -> Parser m String
string = traverse char

digit :: MonadPlus m => Parser m Char
digit = satisfy isDigit

letter :: MonadPlus m => Parser m Char
letter = satisfy isLetter

uint :: MonadPlus m => Parser m Int
uint = read <$> some digit

space :: MonadPlus m => Parser m Char
space = satisfy isSpace

lexeme :: MonadPlus m => Parser m a -> Parser m a
lexeme = (<* many space)

parenthesized :: MonadPlus m => Parser m a -> Parser m a
parenthesized p = lexeme (char '(') *> p <* lexeme (char ')')
