{-# LANGUAGE TypeApplications #-}

module P8 where

import Control.Monad ( forM_ )
import Data.List ( group, sort, minimumBy )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Map as M

(dimW, dimH) = (25, 6)

pixelsPerLayer = dimW * dimH

takeDrop :: Int -> [a] -> ([a], [a])
takeDrop 0 l = ([], l)
takeDrop n [] = ([], [])
takeDrop n (x : xs) = (x : xs', l) where
  (xs', l) = takeDrop (n-1) xs

-- | Divides a list into n-sized chunks.
divide :: Int -> [a] -> [[a]]
divide n [] = []
divide n l = chunk : divide n l' where
  (chunk, l') = takeDrop n l

-- | Calculates a histogram of the elements in the list.
histo :: (Eq a, Ord a) => [a] -> M.Map a Int
histo = M.fromList . map decomp . group . sort where
  decomp l@(x : _) = (x, length l)

-- | Takes a list of layers (first entry topmost) and rearranges it into a list of pixel stacks, first entry topmost.
transp :: [[a]] -> [[a]]
transp l@( (_:_) : _ ) = h : transp t where
  (h, t) = unzip $ map (\ (h : t) -> (h, t)) l
transp _ = []

-- | Collapses a pixel stack to its first visible pixel.
collapse :: [Int] -> Int
collapse [] = error "can't collapse empty"
collapse (x : xs) = case x of
  0 -> 0
  1 -> 1
  2 -> collapse xs

main1 :: IO ()
main1 = do
  h <- minimumBy (comparing $ lookupDefault 0 0) . map histo . divide pixelsPerLayer . map (read @Int . pure) . init <$> readFile "inputs/p8.txt"
  -- one histo for each layer
  print $ lookupDefault 0 1 h * lookupDefault 0 2 h
  where
    lookupDefault d k = fromMaybe d . M.lookup k


main2 :: IO ()
main2 =
  divide dimW . map collapse . transp . divide pixelsPerLayer . map (read @Int . pure) . init <$> readFile "inputs/p8.txt" >>=
  mapM_ (putStrLn . map showD) where
    showD 0 = ' '
    showD 1 = 'X'
