{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module State where

import Trans
import ExceptT

import Control.Monad ( ap )

class Monad m => MonadState m where
  type State (m :: * -> *) :: *
  get :: m (State m)
  put :: State m -> m ()

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

instance Monad m => MonadState (StateT s m) where
  type State (StateT s m) = s
  -- get :: Monad m => StateT m s s
  get = StateT $ \s -> pure (s, s)
  put s = StateT $ \_ -> pure (s, ())

instance MonadTrans (StateT s) where
  lift x = StateT $ \s -> (s,) <$> x

gets :: MonadState m => (State m -> s) -> m s
gets f = f <$> get

modify :: MonadState m => (State m -> State m) -> m ()
modify f = put . f =<< get

instance MonadExcept m => MonadExcept (StateT s m) where
  type Exc (StateT s m) = Exc m
  throwError = lift . throwError
  catchError (StateT x) h = StateT $ \s -> do
    catchError (x s) (flip unState s . h)
