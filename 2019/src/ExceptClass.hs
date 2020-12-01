{-# LANGUAGE TypeFamilies #-}

module ExceptClass where

class Monad m => MonadExcept m where
  type Exc m :: *
  throwError :: Exc m -> m a
  catchError :: m a -> (Exc m -> m a) -> m a
