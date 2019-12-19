{-# LANGUAGE TypeFamilies #-}

module StateClass where

class Monad m => MonadState m where
  type State (m :: * -> *) :: *
  get :: m (State m)
  put :: State m -> m ()

gets :: MonadState m => (State m -> s) -> m s
gets f = f <$> get

modify :: MonadState m => (State m -> State m) -> m ()
modify f = put . f =<< get
