module Day6 (main) where

import Data.List ( nub )
import System.IO

data Race = Race { time :: Integer, distance :: Integer }

races :: [Race]
races =
  [ Race 40 219
  , Race 81 1012
  , Race 77 1365
  , Race 72 1098
  ]

ultimateRace = Race 40817772 219101213651098

-- | Computes how many ways the race can be won.
-- solve :: Race -> Int
solve (Race tmax xbar) = (hi - lo) where
  lo = ceiling $ quad (-)
  hi = ceiling $ quad (+)

  quad f = (t `f` sqrt (t**2 - 4*x)) / 2 where
    t = fromIntegral tmax :: Double
    x = fromIntegral xbar

testRaces =
  [ Race 7 9
  , Race 15 40
  ]

main :: IO ()
main = print $ solve ultimateRace
