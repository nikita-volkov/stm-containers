module STMContainers.HAMT.Node.Nodes where

import STMContainers.Prelude
import qualified STMContainers.WordArray as WordArray
import qualified Focus


type Nodes n = WordArray.WordArray (TVar n)

type Index = WordArray.Index

type Focus n r = Focus.StrategyM STM n r

{-# INLINE lookup #-}
lookup :: Index -> Nodes n -> STM (Maybe n)
lookup i w = maybe (return Nothing) (fmap Just . readTVar) $ WordArray.lookup i w

{-# INLINE insert #-}
insert :: n -> Index -> Nodes n -> STM (Nodes n)
insert n i ns = do
  nv <- newTVar n
  return $ inline WordArray.set i nv ns

{-# INLINE focus #-}
focus :: Focus n r -> Index -> Nodes n -> STM (r, Nodes n)
focus a i = 
  inline WordArray.focusM a' i
  where
    a' = \case
      Just v -> do
        (r, c) <- a . Just =<< readTVar v
        case c of
          Focus.Keep -> return (r, Focus.Keep)
          Focus.Remove -> return (r, Focus.Remove)
          Focus.Replace n' -> writeTVar v n' >> return (r, Focus.Keep)
      Nothing -> do
        (r, c) <- a Nothing
        case c of
          Focus.Replace n' -> newTVar n' >>= \v -> return (r, Focus.Replace v)
          _ -> return (r, Focus.Keep)

{-# INLINE foldM #-}
foldM :: (a -> n -> STM a) -> a -> Nodes n -> STM a
foldM f = inline WordArray.foldM (\acc v -> readTVar v >>= f acc)

{-# INLINE null #-}
null :: Nodes n -> Bool
null = inline WordArray.null

{-# INLINE singleton #-}
singleton :: Index -> n -> STM (Nodes n)
singleton i n = WordArray.singleton i <$> newTVar n

{-# INLINE pair #-}
pair :: Index -> n -> Index -> n -> STM (Nodes n)
pair i n i' n' =
  WordArray.pair <$> pure i <*> newTVar n <*> pure i' <*> newTVar n'
