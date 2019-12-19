{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module StateT
( MonadState(..), 
  StateT(..),
  gets,
  modify
)
where

import Control.Monad ( ap )

import ExceptClass
import MonadIO
import StateClass
import Trans

newtype StateT s m a =
  StateT { unState :: s -> m (s, a) }
  deriving Functor

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (s, x)
  (<*>) = ap

instance Monad m => Monad (StateT s m) where
  return = pure
  StateT f >>= k = StateT $ \s -> do
    (s', x) <- f s
    unState (k x) s'

instance MonadTrans (StateT s) where
  lift x = StateT $ \s -> (s,) <$> x

instance Monad m => MonadState (StateT s m) where
  type State (StateT s m) = s
  -- get :: Monad m => StateT m s s
  get = StateT $ \s -> pure (s, s)
  put s = StateT $ \_ -> pure (s, ())

instance MonadExcept m => MonadExcept (StateT s m) where
  type Exc (StateT s m) = Exc m
  throwError = lift . throwError
  catchError (StateT x) h = StateT $ \s -> do
    catchError (x s) (flip unState s . h)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
