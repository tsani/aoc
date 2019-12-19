module P9 where

import Control.Monad ( forM_, forever )

import P5

main = do
  s <- loadCPU "inputs/p9.txt"
  let (s', _) = runInterp (forever step) s { _input = [2] }
  forM_ (_output s') print
