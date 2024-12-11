{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators #-}

module Homo where

import Data.Kind
import GHC.TypeLits

data Homo (n :: Nat) (a :: Type) :: Type where
  HZ :: Homo 0 a
  (:|) :: a -> Homo n a -> Homo (n + 1) a

infixr 5 :|


