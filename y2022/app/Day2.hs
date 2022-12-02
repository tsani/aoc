module Day2 (main)where

import System.IO

-- we play    | they play    | we score
-- Rock     0 | Paper      1 | 1 + 0 (loss)  = 1
-- Paper    1 | Scissors   2 | 2 + 0 (loss)  = 2
-- Scissors 2 | Rock       0 | 3 + 0 (loss)  = 3
-- Rock     0 | Rock       0 | 1 + 3 (draw)  = 4
-- Paper    1 | Paper      1 | 2 + 3 (draw)  = 5
-- Scissors 2 | Scissors   2 | 3 + 3 (draw)  = 6
-- Rock     0 | Scissors   2 | 1 + 6 (win)   = 7
-- Paper    1 | Rock       0 | 2 + 6 (win)   = 8
-- Scissors 2 | Paper      1 | 3 + 6 (win)   = 9

-- It's basically in ternary. 

data RPS = R | P | S
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data XYZ = X | Y | Z
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

xyz2rps :: XYZ -> RPS
xyz2rps = toEnum . fromEnum

-- | Given what our opponent plays and what we need the round to end in, decide
-- what we need to play.
decidePlay :: RPS -> XYZ -> RPS
decidePlay opp out = f opp where
  f = case out of
    X -> rpsPred
    Y -> id
    Z -> rpsSucc

type Round = (RPS, XYZ)
type Guide = [Round]

parse :: String -> Guide
parse = map (parseLine . words) . lines where
  parseLine [w1, w2] = (code1 $ head w1, code2 $ head w2) where
    code1 'A' = R
    code1 'B' = P
    code1 'C' = S
    code2 'X' = X
    code2 'Y' = Y
    code2 'Z' = Z

playerPoints :: RPS -> Int
playerPoints = (1 +) . fromEnum

rpsPred :: RPS -> RPS
rpsPred = toEnum . (`mod` 3) . (subtract 1) . fromEnum

rpsSucc :: RPS -> RPS
rpsSucc = toEnum . (`mod` 3) . (+ 1) . fromEnum

outcome :: RPS -> RPS -> Int
outcome r1 r2
  | rpsSucc r1 == r2 = 0
  | r1 == r2 = 3
  | rpsPred r1 == r2 = 6

roundScore :: Round -> Int
roundScore (opponent, player) = playerPoints (xyz2rps player) + outcome (xyz2rps player) opponent

tournamentScore :: Guide -> Int
tournamentScore = sum . map roundScore

tournamentScore' :: Guide -> Int
tournamentScore' = sum . map roundScore where
  roundScore (rps, xyz) = playerPoints (decidePlay rps xyz) + xyzScore xyz where
    xyzScore x = case x of
      X -> 0
      Y -> 3
      Z -> 6

answer1 = tournamentScore . parse
answer2 = tournamentScore' . parse

main = withFile "input/day2.txt" ReadMode $ \h ->
  print =<< answer2 <$> hGetContents h
