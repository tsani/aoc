{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Applicative
import Control.Monad ( ap, void )
import Data.Functor.Identity
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N

import ExceptT
import StateT
import Trans

data ParserState =
  ParserState
  { input :: String
  }

_input :: (String -> String) -> ParserState -> ParserState
_input f s = s { input = f (input s) }

newtype Parser a =
  Parser { unParser :: StateT ParserState (ExceptT () Identity) a }
  deriving (Functor, Applicative, Monad)

parse :: String -> Parser a -> Maybe a
parse s p = either (const Nothing) (Just . snd) $ runIdentity $ unExcept (unState (unParser p) s') where
  s' = ParserState s

instance MonadState Parser where
  type State Parser = ParserState
  get = Parser get
  put s = Parser (put s)

instance MonadExcept Parser where
  type Exc Parser = ()
  throwError = Parser . throwError
  catchError (Parser x) h = Parser $ do
    catchError x (unParser . h)

instance Alternative Parser where
  empty = throwError ()
  m1 <|> m2 = m1 `catchError` \_ -> m2

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- gets input
  case s of
    c : cs | p c -> modify (_input (const cs)) *> pure c
    _ -> throwError ()

lower :: Parser Char
lower = choice $ map char "qwertyuiopasdfghjklzxcvbnm"

upper :: Parser Char
upper = choice $ map char "QWERTYUIOPASDFGHJKLZXCVBNM"

char :: Char -> Parser Char
char c = satisfy (c ==)

sepBy1 :: Parser a -> Parser () -> Parser (NonEmpty a)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p sep = N.toList <$> sepBy1 p sep <|> pure []

eof :: Parser ()
eof = gets input >>= \case
  [] -> pure ()
  _ -> throwError ()

string :: String -> Parser ()
string [] = pure ()
string (c : cs) = char c *> string cs

digit :: Parser Int
digit = choice choices where
  choices = map f table where
    f (c, d) = char c *> pure d
  table =
    [ ('0', 0)
    , ('1', 1)
    , ('2', 2)
    , ('3', 3)
    , ('4', 4)
    , ('5', 5)
    , ('6', 6)
    , ('7', 7)
    , ('8', 8)
    , ('9', 9)
    ]

positiveInteger :: Parser Int
positiveInteger = fromDigits 0 <$> some digit where
  fromDigits acc [] = acc
  fromDigits acc (d : ds) = fromDigits (10 * acc + d) ds

integer :: Parser Int
integer = do
  sign <- maybe 1 (const (-1)) <$> optional (char '-')
  n <- positiveInteger
  pure (sign * n)
