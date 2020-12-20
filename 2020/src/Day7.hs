{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Pretty.Simple ( pPrint )

import qualified WG

type Color = T.Text

type Bags' = WG.WG' Color Int
type Bags = WG.WG Color Int

type Path = [Color]

p1 :: IO ()
p1 = pPrint =<< go <$> T.readFile "input/day7.txt" where
  go = subtract 1 . length . WG.elim phi "shiny gold" . WG.invert . parse

  phi :: Color -> [(Int, S.Set Color)] -> S.Set Color
  phi k [] = S.singleton k
  phi k rs = S.singleton k `S.union` S.unions (snd <$> rs)

p2 :: IO ()
p2 = pPrint =<< go <$> T.readFile "input/day7.txt" where
  go = subtract 1 . WG.elim phi "shiny gold" . parse

  phi :: Color -> [(Int, Int)] -> Int
  phi _ rs = 1 + sum (map (uncurry (*)) rs)

parse :: T.Text -> Bags
parse = WG.WG . M.unions . map (parseLine . T.words) . T.lines where
  parseLine :: [T.Text] -> Bags'
  parseLine (c1:c2:_:_:rest) = M.singleton (c1 <> " " <> c2) value where
    value = case rest of
      xs | xs == ["no", "other", "bags."] -> []
      xs -> (bagSpec <$> divide 4 rest)

    bagSpec (count:c1:c2:_) = (read (T.unpack count), c1 <> " " <> c2)

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = let (y, xs') = splitAt n xs in y : divide n xs'
