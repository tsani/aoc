{-# LANGUAGE DeriveFunctor #-}

module Parser
  ( Parser(..)
  , satisfy
  , int
  , spaces
  , lineBreak
  , parseOnly
  , unsafeParseOnly
  , sepBy
  , sepBy1
  , module Control.Applicative )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe

unsafeParseOnly :: Parser a -> String -> a
unsafeParseOnly p = fromJust . parseOnly p

parseOnly :: Parser a -> String -> Maybe a
parseOnly p s = snd <$> runParser (p <* eof) s

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
  deriving Functor

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  Parser f <*> Parser x = Parser $ \s -> do
    (s', f') <- f s
    (s'', x') <- x s'
    pure (s'', f' x')

instance Monad Parser where
  (Parser m) >>= k = Parser $ \s -> do
    (s', x) <- m s
    runParser (k x) s'

instance Alternative Parser where
  empty = Parser $ \_ -> empty
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  c:cs -> if p c then pure (cs, c) else empty
  _ -> empty

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

int :: Parser Int
int = snd . foldr (\d (n, a) -> (n*10, a + d*n)) (1, 0) <$> some digit

space :: Parser ()
space = void $ satisfy isSeparator

spaces :: Parser ()
spaces = void $ some space

lineBreak :: Parser ()
lineBreak = void $ satisfy ('\n' ==)

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> pure ([], ())
  _ -> empty

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> pure Nothing

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

sepBy1 :: Parser a -> Parser () -> Parser [a]
sepBy1 p sep = sepBy p sep <|> pure []
