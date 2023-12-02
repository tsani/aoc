{-# LANGUAGE NamedFieldPuns #-}

module Day2 where

import Control.Applicative
import Data.Maybe (fromJust)
import Data.Foldable
import Data.Functor
import System.IO

import Parser

type Id = Int

data Outcome = Outcome { red :: Int, green :: Int, blue :: Int }
  deriving (Show)

instance Semigroup Outcome where
  Outcome r1 g1 b1 <> Outcome r2 g2 b2 = 
    Outcome (max r1 r2) (max g1 g2) (max b1 b2)
  
instance Monoid Outcome where
  mempty = Outcome 0 0 0

data Game = Game Id [Outcome]
  deriving Show

parseGame :: Parser Game
parseGame = do
  string "Game "
  n <- number
  string ": "
  outcomes <- parseOutcome `sepBy` string "; "
  pure $ Game n outcomes

parseOutcome :: Parser Outcome
parseOutcome = foldr ($) mempty <$> (color `sepBy` string ", ") where
  color :: Parser (Outcome -> Outcome)
  color = do
    n <- number
    string " "
    (string "red" $> \o -> o { red = n })
      <|> (string "green" $> \o -> o { green = n })
      <|> (string "blue" $> \o -> o { blue = n })

isPossibleOutcome :: Outcome -> Bool
isPossibleOutcome (Outcome { red, green, blue }) = red <= 12 && green <= 13 && blue <= 14

isPossibleGame :: Game -> Bool
isPossibleGame (Game _ outcomes) = all isPossibleOutcome outcomes

power :: Outcome -> Int
power (Outcome r g b) = r * g * b

answer2 = sum . map power .
  map (\(Game _ outcomes) -> foldMap id outcomes) .
  parseLines

answer1 = sum .
  map (\(Game n _) -> n) . filter isPossibleGame .
  parseLines

parseLines = 
  map (snd . fromJust . runParser parseGame) .
  lines

debug :: String -> IO ()
debug input = for_ (lines input) $ \line -> do
  putStrLn $ "parsing: " ++ line
  print $ runParser parseGame line

main = withFile "input/day2.txt" ReadMode $ \h ->
  -- hGetContents h >>= debug
  print =<< answer2 <$> hGetContents h
