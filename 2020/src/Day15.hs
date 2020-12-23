{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day15 where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M

import Prelude hiding ( last )

type When = Int

data State = State
  { last :: !Int
  -- ^ The last number spoken
  , prevs :: !(Map Int When)
  -- ^ All numbers spoken before that, sent to the turn number when they were spoken
  , turn :: !When
  -- ^ The current turn number
  }

input = [2,0,6,12,1,3]

initialState :: [Int] -> State
initialState [] = error "impossible"
initialState l = go M.empty $ zip [1..] l where
  go prevs [(turn, last)] = State {..}
  go prevs ((turn, last) : l) = go (M.insert last turn prevs) l

step :: State -> State
step State{..} = case M.lookup last prevs of
  Nothing -> State { last = 0, prevs = M.insert last turn prevs, turn = turn + 1 }
  Just w -> State { last = turn - w, prevs = M.insert last turn prevs, turn = turn + 1 }

seqFrom :: [Int] -> [Int]
seqFrom i = (init i ++) . map last . iterate step . initialState $ i

answer1 :: Int
answer1 = seqFrom input !! (2020 - 1)

answer2 :: Int
answer2 = seqFrom input !! (30000000 - 1)
