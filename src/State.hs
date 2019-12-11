{-# LANGUAGE DeriveFunctor #-}

module State where

import Control.Monad ( ap )

newtype State s a =
  State { unState :: s -> (s, a) }
  deriving Functor

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  (<*>) = ap

instance Monad (State s) where
  return = pure
  State f >>= k = State $ \s ->
    let (s', x) = f s in
    unState (k x) s'
  
get :: State s s
get = State $ \s -> (s, s)

gets :: (s -> s') -> State s s'
gets f = f <$> get

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())
