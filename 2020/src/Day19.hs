{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day19 where

import Control.Applicative
import Control.Monad ( void, MonadPlus )
import Data.List ( intercalate )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, fromJust, isJust )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void

import Prelude hiding ( seq )

import Parser

data Rule' a
  = Char Char
  | Ref a
  | Seq (Rule' a) (Rule' a)
  | Alt (Rule' a) (Rule' a)

type RawRule = Rule' Int
type Rule = Rule' Void
type RawRules = Map Int RawRule

p1 :: IO ()
p1 = print . length . filter isJust . go . parse =<< T.readFile "input/day19.txt" where
  go Problem { rawRules, messages } = runParser (only $ compile (rule0 rawRules)) <$> messages

p2 :: IO ()
p2 = print . length . filter (not . null @[]) . go . parse
  =<< T.readFile "input/day19-part2.txt" where
  go Problem { rawRules, messages } = runParser (only $ compile (rule0 rawRules)) <$> messages

-- | Compiles a set of raw rules into a single rule starting at 0.
rule0 :: RawRules -> Rule
rule0 rules = goRule (rules M.! 0) where
  goRule :: RawRule -> Rule
  goRule = \case
    Char c -> Char c
    Seq r1 r2 -> Seq (goRule r1) (goRule r2)
    Alt r1 r2 -> Alt (goRule r1) (goRule r2)
    Ref i -> goRule (rules M.! i)

-- | Compiles a rule into a parser that recognizes that rule.
compile :: MonadPlus m => Rule -> Parser m String
compile = \case
  Char c -> pure <$> char c
  Ref r -> absurd r
  Seq r1 r2 -> (++) <$> compile r1 <*> compile r2
  Alt r1 r2 -> compile r1 <|> compile r2

data Problem = Problem { rawRules :: RawRules, messages :: [String] }

parse :: Text -> Problem
parse (T.splitOn "\n\n" -> map (lines . T.unpack) -> [rawRuleStrs, messages]) = Problem
  { rawRules = M.fromList (parseRawRule <$> rawRuleStrs)
  , messages
  } where
  parseRawRule :: String -> (Int, RawRule)
  parseRawRule = snd . fromJust . runParser prefixedRule
parse _ = error "invalid input"

prefixedRule :: MonadPlus m => Parser m (Int, RawRule)
prefixedRule = (,) <$> uint <* lexeme (char ':') <*> rawRule where
  rawRule :: MonadPlus m => Parser m RawRule
  rawRule = alt where
    alt = Alt <$> (seq <* lexeme (char '|')) <*> alt <|> seq
    seq = Seq <$> atom <*> seq <|> atom
    atom = lexeme (Char <$> (char '"' *> letter <* char '"')) <|> lexeme (Ref <$> uint)
