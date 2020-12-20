module Comonad where

class Functor w => Comonad w where
  duplicate :: w a -> w (w a)

  extract :: w a -> a

  cobind :: w a -> (w a -> a) -> w a
  cobind w k = k <$> duplicate w
