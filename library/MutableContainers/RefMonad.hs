module MutableContainers.RefMonad where

import MutableContainers.Prelude


class (Monad m) => RefMonad m where
  type Ref m
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()
  modifyRef :: Ref m a -> (a -> a) -> m ()

instance RefMonad IO where
  type Ref IO = IORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef

instance RefMonad (ST s) where
  type Ref (ST s) = STRef s
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef

instance RefMonad STM where
  type Ref STM = TVar
  readRef = readTVar
  writeRef = writeTVar
  modifyRef var f = do
    x <- readTVar var
    writeTVar var $! f x

