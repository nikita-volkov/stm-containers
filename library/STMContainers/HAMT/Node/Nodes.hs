module STMContainers.HAMT.Node.Nodes where

import STMContainers.Prelude
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.Focus as Focus


type Nodes n = WordArray.WordArray (TVar n)

type Index = WordArray.Index

type Focus n r = Focus.FocusM STM n r

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

foldM :: (a -> n -> STM a) -> a -> Nodes n -> STM a
foldM f = inline WordArray.foldM (\acc v -> readTVar v >>= f acc)

null :: Nodes n -> Bool
null = inline WordArray.null

singleton :: Index -> n -> STM (Nodes n)
singleton i n = WordArray.singleton i <$> newTVar n

pair :: Index -> n -> Index -> n -> STM (Nodes n)
pair i n i' n' =
  WordArray.pair <$> pure i <*> newTVar n <*> pure i' <*> newTVar n'
