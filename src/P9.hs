module P9 where

import Control.Monad ( forM_ )

import P5

main = do
  s <- loadState "inputs/p9.txt"
  let (s', _) = unInterp trace s { _input = [2] }
  forM_ (_output s') print
