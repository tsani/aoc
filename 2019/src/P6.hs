{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module P6 where

import Data.Bifunctor ( bimap )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( catMaybes, listToMaybe )

newtype Obj = Obj { unObj :: String }
  deriving (Eq, Ord, Show)

instance Read Obj where
  readsPrec _ (c1:c2:c3:s) =
    pure (Obj [c1,c2,c3], s)

data Orbit
  = Orbit
    { orbitee :: Obj
    , orbiter :: Obj
    }
  deriving Show

instance Read Orbit where
  readsPrec _ s = do
    (orbitee, s) <- reads s
    s <- case s of
      ')' : s -> pure s
      _ -> []
    (orbiter, s) <- reads s
    pure (Orbit {..}, s)

type OrbitMap = M.Map Obj (S.Set Obj)

insertOrbit :: Orbit -> OrbitMap -> OrbitMap
insertOrbit Orbit{..} =
  M.alter
    (pure . maybe (S.singleton orbiter) (S.insert orbiter))
    orbitee

foldOrbitMap :: forall a. (Obj -> [a] -> a) -> OrbitMap -> a
foldOrbitMap phi g = go (Obj "COM") (orbiters (Obj "COM") g) where
  go :: Obj -> [Obj] -> a
  go o = phi o . map (\o -> go o $ orbiters o g)

orbiters :: Obj -> OrbitMap -> [Obj]
orbiters k = maybe [] S.toList . M.lookup k

countOrbits :: OrbitMap -> Int
countOrbits = flip (foldOrbitMap (const phi)) 0 where
  phi :: [Int -> Int] -> Int -> Int
  phi fs k = sum $ k : sequence fs (k+1)

-- Idea: count the number of transfer from YOU to the (LCA-1) and SAN to
-- (LCA-1) where LCA is the least common ancestor of YOU and SAN.
-- I write LCA-1 because we aren't counting from SAN to LCA, we're
-- counting from SAN to an object *orbiting* LCA. This is equivalent
-- to counting from the object SAN is orbiting to LCA.
transferCount :: OrbitMap -> (Maybe Int, Maybe Int)
transferCount = foldOrbitMap phi where
  phi :: Obj -> [(Maybe Int, Maybe Int)] -> (Maybe Int, Maybe Int)
  phi (Obj "YOU") _ = (Just 0, Nothing)
  phi (Obj "SAN") _ = (Nothing, Just 0)
  -- both distances are known
  phi _ (unzip -> bimap catMaybes catMaybes -> ([k1], [k2])) =
    (Just k1, Just k2) -- no more incrementing
    -- This first time this happens we are at the closest common
    -- ancestor of the SAN and YOU.
  -- otherwise increment the distance we know
  phi _ l = bimap f f $ unzip l where
    f = listToMaybe . map (+1) . catMaybes

loadMap :: IO OrbitMap
loadMap =
  foldr insertOrbit M.empty .
  map read . lines <$> readFile "inputs/p6.txt"

main1 = do
  print =<< countOrbits <$> loadMap

main2 = do
  (Just k1, Just k2) <- transferCount <$> loadMap
  print (k1 + k2)
