{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module ExceptT where

import Trans

import Control.Monad ( ap, join )

class Monad m => MonadExcept m where
  type Exc m :: *
  throwError :: Exc m -> m a
  catchError :: m a -> (Exc m -> m a) -> m a

newtype ExceptT e m a =
  ExceptT { unExcept :: m (Either e a) }
  deriving Functor

instance Monad m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
  return = pure
  ExceptT f >>= k = ExceptT $ f >>= either (pure . Left) (unExcept . k)

instance Monad m => MonadExcept (ExceptT e m) where
  type Exc (ExceptT e m) = e
  throwError = ExceptT . pure . Left
  catchError (ExceptT f) handle =
    ExceptT $ unExcept . either handle pure =<< f

instance MonadTrans (ExceptT e) where
  lift x = ExceptT $ pure <$> x
