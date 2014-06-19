module STMContainers.HAMT.Node.Nodes where

import STMContainers.Prelude
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.Alter as Alter


type Nodes n = WordArray.WordArray (TVar n)

type Index = WordArray.Index

type Alter n r = Alter.AlterM STM n r

alterM :: Alter n r -> Index -> Nodes n -> STM (r, Nodes n)
alterM a i = 
  inline WordArray.alterM a' i
  where
    a' = \case
      Just v -> do
        (r, c) <- a . Just =<< readTVar v
        case c of
          Alter.Keep -> return (r, Alter.Keep)
          Alter.Remove -> return (r, Alter.Remove)
          Alter.Replace n' -> writeTVar v n' >> return (r, Alter.Keep)
      Nothing -> do
        (r, c) <- a Nothing
        case c of
          Alter.Replace n' -> newTVar n' >>= \v -> return (r, Alter.Replace v)
          _ -> return (r, Alter.Keep)

foldM :: (a -> n -> STM a) -> a -> Nodes n -> STM a
foldM f = inline WordArray.foldM (\acc v -> readTVar v >>= f acc)

null :: Nodes n -> Bool
null = inline WordArray.null

fromSizedList :: (Int, [(Index, n)]) -> STM (Nodes n)
fromSizedList (size, list) = 
  inline WordArray.fromSizedListM (size, list')
  where
    list' = map (\(i, n) -> fmap (i,) (newTVar n)) list
