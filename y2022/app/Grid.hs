{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable, NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE TypeFamilies, DataKinds, GADTs, DerivingStrategies, DerivingVia, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, GeneralizedNewtypeDeriving, RankNTypes, ConstraintKinds #-}

module Grid
   ( Grid(..)
   , Loc
   , Dir(..)
   , dirs
   , GridZ
   , sizedGrid
   , gridz
   , gridz0
   , dir
   , jump
   , move
   , lookEverywhere
   , materialize
   , Fin
   , unFin
   , fin
   , fin'
   , KnownSize
   , module Control.Comonad
   , module Control.Comonad.Representable.Store
   ) where

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Bitraversable
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Rep
import Data.Functor.Classes
import Data.Distributive
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Vector as V
import GHC.TypeLits

newtype SizedVector (n :: Nat) a = SizedVector { unSizedVector :: V.Vector a }
  deriving (Functor, Foldable, Traversable, Show )
  deriving newtype Show1

-- data SomeSizedVector a where
--   SomeSizedVector :: KnownNat n => SizedVector n a -> SomeSizedVector a

withSized :: V.Vector a -> (forall n. KnownNat n => SizedVector n a -> r) -> r
withSized v k = case fromJust . someNatVal . fromIntegral $ V.length v of
  SomeNat (Proxy :: Proxy n) -> k $ SizedVector @n v

type KnownSize h w = (KnownNat h, KnownNat w)

-- | Constructs a grid from nested vectors. The inner vectors must all have the
-- same length, and the outer vector must be nonempty.
sizedGrid :: V.Vector (V.Vector a) -> (forall h w. KnownSize h w => Grid h w a -> r) -> r
sizedGrid v k =
  withSized v $ \(_ :: SizedVector h (V.Vector a)) ->
  withSized (v V.! 0) $ \(_ :: SizedVector w a) ->
  k $ Grid @h @w (coerce v)

newtype Fin (n :: Nat) = Fin { unFin :: Int }
  deriving (Eq, Ord, Show)

fin :: forall n. KnownNat n => Int -> Maybe (Fin n)
fin i
  | 0 <= i && i < fromIntegral (natVal $ Proxy @n) = Just (Fin i)
  | otherwise = Nothing

-- | Unsafely construct a bounded integer.
fin' :: Int -> Fin n
fin' = Fin

instance forall n. KnownNat n => Distributive (SizedVector n) where
  distribute f =
    SizedVector . V.map (\i -> (V.! i) . unSizedVector <$> f) $
    V.generate (fromIntegral . natVal $ Proxy @n) id

instance forall n. KnownNat n => Representable (SizedVector n) where
  type Rep (SizedVector n) = Fin n

  index (SizedVector v) (Fin i) = v V.! i
  tabulate f = SizedVector . V.map (f . Fin) $ V.generate (fromIntegral . natVal $ Proxy @n) id

newtype Grid (h :: Nat) (w :: Nat) a = Grid { unGrid :: Compose (SizedVector h) (SizedVector w) a }
  deriving (Functor, Foldable, Traversable, Show )

instance KnownSize h w => Distributive (Grid h w) where
  distribute f = Grid (distribute $ unGrid <$> f)

instance KnownSize h w => Representable (Grid h w) where
  type Rep (Grid h w) = (Fin h, Fin w)

  index (Grid (Compose (SizedVector g))) (Fin i, Fin j) = unSizedVector (g V.! i) V.! j
  tabulate f = Grid $ tabulate f

data Dir a = Dir { up :: a, down :: a, left :: a, right :: a }
  deriving (Functor, Foldable, Traversable, Show)

type Direction a = Dir a -> a

dirs :: Dir (Direction a)
dirs = Dir { up = up, down = down, left = left, right = right }

dirVecs :: Dir (Int, Int)
dirVecs = Dir { up = (-1, 0), down = (1, 0), left = (0, -1), right = (0, 1) }

instance Applicative Dir where
  pure x = Dir { up = x, down = x, left = x, right = x }
  f <*> x = Dir
    { up = up f (up x)
    , down = down f (down x)
    , right = right f (right x)
    , left = left f (left x)
    }

type Loc h w = (Fin h, Fin w)

type GridZ h w a = Store (Grid h w) a

gridz :: KnownSize h w  => Loc h w -> Grid h w a -> GridZ h w a
gridz loc g = store (index g) loc

gridz0 :: KnownSize h w => Grid h w a -> GridZ h w a
gridz0 = gridz (Fin 0, Fin 0)

-- | Gets the grids that result from moving in each direction.
dir :: forall h w a. KnownSize h w  => GridZ h w a -> Dir (Maybe (GridZ h w a))
dir g = jump <$> ((+++) <$> pure (i, j) <*> dirVecs) <*> pure g where
  (Fin !i, Fin !j) = pos g
  (!x1, !y1) +++ (!x2, !y2) = (x1 + x2, y1 + y2)

-- | Perform an absolute movement in a grid.
jump :: forall h w a. KnownSize h w  => (Int, Int) -> GridZ h w a -> Maybe (GridZ h w a)
jump (!i, !j) g = seek <$> bisequenceA (fin i, fin j) <*> pure g

-- | Perform multiple relative movements in a grid.
move :: KnownSize h w => (Int, Int) -> GridZ h w a -> Maybe (GridZ h w a)
move (0, 0) g = Just g
move (0, dj) g
  | dj > 0 = move (0, dj - 1) =<< (right . dir) g
  | dj < 0 = move (0, dj + 1) =<< (left . dir) g
move (di, dj) g
  | di > 0 = move (di - 1, dj) =<< (down . dir) g
  | dj < 0 = move (di + 1, dj) =<< (up . dir) g

-- | Look in each direction from the given point and return a list of elements
-- seen in each direction.
lookEverywhere :: KnownSize h w => GridZ h w a -> Dir [a]
lookEverywhere g = f <$> dirs <*> dir g where
  f d = maybe [] (\g -> extract g : f d (d . dir $ g))

materialize :: KnownSize h w => GridZ h w a -> [[a]]
materialize = rows . seek (Fin 0, Fin 0) where
  row g = extract g : maybe [] row (right . dir $ g)
  rows g = row g : maybe [] rows (down . dir $ g)
