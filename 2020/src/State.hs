{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}

module State where

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

  modify :: (s -> s) -> m ()
  modify f = put . f =<< get

  gets :: (s -> a) -> m a
  gets f = f <$> get

newtype State s a = State { runState :: s -> (s, a) } deriving Functor

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  State f <*> State x = State $ \s ->
    let (s', f') = f s in
    let (s'', x') = x s' in
    (s'', f' x')

instance Monad (State s) where
  return = pure
  State x >>= k = State $ \s ->
    let (s', x') = x s in
    k x' `runState` s'

instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  put s = State $ \_ -> (s, ())
