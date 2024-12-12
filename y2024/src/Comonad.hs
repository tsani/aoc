module Comonad where

class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate w = extend w id

  extend :: w a -> (w a -> b) -> w b
  extend w f = f <$> duplicate w
