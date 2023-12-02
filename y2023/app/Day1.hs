{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day1 where

import System.IO
import Control.Applicative
import Data.Char (isDigit)

data DigitWord = DigitWord { digit :: Int, word :: String }
digitWords = map (uncurry DigitWord)
  [ (1, "1"), (1, "one")
  , (2, "2"), (2, "two")
  , (3, "3"), (3, "three")
  , (4, "4"), (4, "four")
  , (5, "5"), (5, "five")
  , (6, "6"), (6, "six")
  , (7, "7"), (7, "seven")
  , (8, "8"), (8, "eight")
  , (9, "9"), (9, "nine")
  ]

processDigitWord :: DigitWord -> Process
processDigitWord (DigitWord n s) = go s where
  go [] = Output n
  go (c:cs) = Consume $ \case
    Nothing -> Fail
    Just c' -> if c == c' then go cs else Fail

processAnyDigitWord :: Process
processAnyDigitWord = Fork (map processDigitWord digitWords)

-- parseAnyWord :: String -> Maybe (Int, String)
-- parseAnyWord s = foldr1 (<|>) $ map (parseWord s) digitWords where
--   parseWord :: String -> DigitWord -> Maybe (Int, String)
--   parseWord s (DigitWord n []) = Just (n, s)
--   parseWord [] _ = Nothing
--   parseWord (c':cs') (DigitWord n (c:cs))
--     | c' == c = parseWord cs' (DigitWord n cs)
--     | otherwise = Nothing

data Process = Fork [Process] | Consume (Maybe Char -> Process) | Output Int | Fail
type State = [Process]

lineToNumber2 = lookingForFirst [] where
  lookingForFirst states input = go [] (processAnyDigitWord : states) where 
    go newStates states = case (input, states) of
      (input, Fork ps' : ps) -> go newStates (ps' ++ ps)
      (input, Output n : ps) -> lookingForSecond n [] input
      (input, Fail : ps) -> go newStates ps

      ([], []) -> error "wot"
      (_:cs, []) -> lookingForFirst newStates cs
      (c:cs, Consume k : ps) -> go (k (Just c) : newStates) ps
      ([], Consume k : ps) -> go (k Nothing : newStates) ps

  lookingForSecond n1 states input = go [] (processAnyDigitWord : states) where
    go newStates states = case (input, states) of
      ([], []) -> done n1 n1
      (_:cs, []) -> lookingForSecond n1 newStates cs
      ([], Consume k : ps) -> go (k Nothing : newStates) ps
      (c:cs, Consume k : ps) -> go (k (Just c) : newStates) ps

      (input, Fork ps' : ps) -> go newStates (ps' ++ ps)
      (input, Output n : ps) -> lookingForLast n1 n [] input
      (input, Fail : ps) -> go newStates ps

  lookingForLast n1 n2 states input = go n2 [] (processAnyDigitWord : states) where
    go n2 newStates states = case (input, states) of
      ([], []) -> done n1 n2
      (_:cs, []) -> lookingForLast n1 n2 newStates cs
      ([], Consume k : ps) -> go n2 (k Nothing : newStates) ps
      (c:_, Consume k : ps) -> go n2 (k (Just c) : newStates) ps

      (input, Fork ps' : ps) -> go n2 newStates (ps' ++ ps)
      (input, Output n : ps) -> go n newStates ps -- seen another number, replace n2
      (input, Fail : ps) -> go n2 newStates ps

  done n1 n2 = n1 * 10 + n2

answer2 :: String -> String
answer2 = show . sum . map lineToNumber2 . lines where

answer1 :: String -> String
answer1 = show . sum . map lineToNumber . lines

lineToNumber = lookingForFirst where
  lookingForFirst [] = error "wot"           -- impossible: no digits in the line
  lookingForFirst [c] = done (see c) (see c) -- last char must be a digit then
  lookingForFirst (c:cs)
    | isDigit c = lookingForSecond (see c) cs -- see the first digit
    | otherwise = lookingForFirst cs          -- keep looking for first digit

  lookingForSecond n1 (c:cs)
    | isDigit c = lookingForLast n1 (see c) cs -- see second digit
    | otherwise = lookingForSecond n1 cs       -- keep looking for second digit
  lookingForSecond n1 [] = done n1 n1          -- first digit is last digit!

  lookingForLast n1 n2 (c':cs)                   -- seen at >=2 digits, look for more
    | isDigit c' = lookingForLast n1 (see c') cs -- see another digit, replace last digit
    | otherwise = lookingForLast n1 n2 cs        -- keep looking for last digit
  lookingForLast n1 n2 [] = done n1 n2
  
  done n1 n2 = n1 * 10 + n2

see :: Char -> Int
see = read . pure

solve solution path = withFile path ReadMode $ \h -> do
  putStrLn =<< solution <$> hGetContents h

main :: IO ()
main = solve answer2 "input/day1.txt"
