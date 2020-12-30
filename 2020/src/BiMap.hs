{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module BiMap where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Set ( Set )
import qualified Data.Set as S

import Prelude hiding ( reverse )

type BiOrd k1 k2 = (Ord k1, Ord k2)

type SetMap k a = Map k (Set a)

-- | A two-way multi-map between sets.
data BiMap k1 k2 = BiMap (SetMap k1 k2) (SetMap k2 k1)

empty = BiMap M.empty M.empty

-- | For each (K1, K2) in l,
-- for each k1 in K1, (resp. k2 in K2),
-- lookupF k1 (fromList l) = K2 (resp. lookupB k2 (fromList l) = K1).
fromList :: forall k1 k2. BiOrd k1 k2 => [(Set k1, Set k2)] -> BiMap k1 k2
fromList = foldr f empty where
  f :: (Set k1, Set k2) -> BiMap k1 k2 -> BiMap k1 k2
  f (s1, s2) (BiMap m1 m2) = BiMap m1' m2' where
    m1' = foldr (\k1 -> M.insertWith S.union k1 s2) m1 (S.toList s1)
    m2' = foldr (\k2 -> M.insertWith S.union k2 s1) m2 (S.toList s2)

reverse :: BiMap k1 k2 -> BiMap k2 k1
reverse (BiMap m1 m2) = BiMap m2 m1

reversedly :: (BiMap k1 k2 -> BiMap k1 k2) -> BiMap k2 k1 -> BiMap k2 k1
reversedly f = reverse . f . reverse

-- | Inserts a mapping both ways.
-- k2 `S.member` lookupF k1 (insert k1 k2 m)
-- k1 `S.member` lookupB k2 (insert k1 k2 m)
insert :: BiOrd k1 k2 => k1 -> k2 -> BiMap k1 k2 -> BiMap k1 k2
insert k1 k2 (BiMap m1 m2) = BiMap (i k1 k2 m1) (i k2 k1 m2) where
  i x y = M.insertWith S.union x (S.singleton y)

lookupF :: BiOrd k1 k2 => k1 -> BiMap k1 k2 -> Set k2
lookupF k1 (BiMap m1 _) = maybe S.empty id (M.lookup k1 m1)

lookupB :: BiOrd k1 k2 => k2 -> BiMap k1 k2 -> Set k1
lookupB k2 = lookupF k2 . reverse

-- | Deletes a mapping both ways.
-- if k2 `S.member` lookupF k1 m,
-- then k2 `S.notMember` lookupF k1 m; and vice-versa.
delete :: BiOrd k1 k2 => k1 -> k2 -> BiMap k1 k2 -> BiMap k1 k2
delete k1 k2 (BiMap m1 m2) = BiMap (d k1 k2 m1) (d k2 k1 m2) where
  d x y = M.adjust (S.delete y) x

-- | Retrieves the left side.
left :: BiMap k1 k2 -> [k1]
left (BiMap m1 _) = M.keys m1

-- | Retrieves the right side.
right :: BiMap k1 k2 -> [k2]
right (BiMap _ m2) = M.keys m2
