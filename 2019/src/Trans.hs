module Trans where

class MonadTrans t where
  lift :: Monad n => n a -> t n a
