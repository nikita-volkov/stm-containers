module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.HAMT.Node as Node
import qualified Focus


type HAMT e = TVar (Node.Node e)

type Element e = (Node.Element e, Hashable (Node.ElementKey e))

{-# INLINABLE insert #-}
insert :: (Element e) => e -> HAMT e -> STM ()
insert e h =
  readTVar h >>=
  Node.insert e (hash (Node.elementKey e)) (Node.elementKey e) 0 >>=
  maybe (return ()) (writeTVar h)

{-# INLINABLE focus #-}
focus :: (Element e) => Focus.StrategyM STM e r -> Node.ElementKey e -> HAMT e -> STM r
focus f k v = do
  n <- readTVar v
  ((r, c), n') <- inline Node.focus (fmap exportCommand . f) (hash k) k 0 n
  case c of
    Focus.Keep -> return ()
    _ -> writeTVar v n'
  return r
  where
    exportCommand (r, c) = ((r, c), c)

{-# INLINABLE foldM #-}
foldM :: (a -> e -> STM a) -> a -> HAMT e -> STM a
foldM step acc = readTVar >=> inline Node.foldM step acc 0

{-# INLINABLE new #-}
new :: STM (HAMT e)
new = newTVar Node.Empty

{-# INLINABLE null #-}
null :: HAMT e -> STM Bool
null v = readTVar v >>= \case
  Node.Empty -> return True
  _ -> return False
