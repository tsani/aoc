{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module ExceptT
( ExceptT(..),
  MonadExcept(..)
) where

import Control.Monad ( ap, join )

import ExceptClass
import MonadIO
import StateClass
import Trans

newtype ExceptT e m a =
  ExceptT { unExcept :: m (Either e a) }
  deriving Functor

instance Monad m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
  return = pure
  ExceptT f >>= k = ExceptT $ f >>= either (pure . Left) (unExcept . k)

instance MonadTrans (ExceptT e) where
  lift x = ExceptT $ pure <$> x

instance Monad m => MonadExcept (ExceptT e m) where
  type Exc (ExceptT e m) = e
  throwError = ExceptT . pure . Left
  catchError (ExceptT f) handle =
    ExceptT $ unExcept . either handle pure =<< f

instance MonadState m => MonadState (ExceptT e m) where
  type State (ExceptT e m) = State m
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO
