{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module WG where

import Control.Monad.State
import qualified Data.Map.Strict as M

type WG'' k1 k2 w = M.Map k1 [(w, k2)]
type WG' k w = WG'' k k w
newtype WG k w = WG { unWG :: WG' k w } deriving (Eq, Ord, Show)

-- | Eliminates a weighted DAG starting from given node.
elim :: forall k w r. Ord k => (k -> [(w, r)] -> r) -> k -> WG k w -> r
elim phi i (WG w) = fst $ runState (go w i) M.empty where
  go :: (StateType m ~ M.Map k r, MonadState m) => WG' k w -> k -> m r
  go g i =
    gets (M.lookup i) >>= \case
      Nothing -> do
        x <- phi i <$> mapM (\(w, j) -> (w,) <$> go g j) (w M.! i)
        modify (M.insert i x)
        pure x
      Just x -> pure x

-- | Inverts all the edges in the graph.
invert :: forall k w. Ord k => WG k w -> WG k w
invert = WG . go . unWG where
  go :: WG' k w -> WG' k w
  go = foldr f M.empty . M.toList where
    f :: (k, [(w, k)]) -> WG' k w -> WG' k w
    f (k, es) g = foldr f' (M.insertWith (++) k [] g) es where
      f' :: (w, k) -> WG' k w -> WG' k w
      f' (w, k') g = M.insertWith (++) k' [(w, k)] g
      -- k ->(w) k'
      -- but what if es = []???
      -- then k -> [] !
