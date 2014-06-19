module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.HAMT.Node as Node
import qualified STMContainers.Alter as Alter


type HAMT e = TVar (Node.Node e)

type Element e = (Node.Element e, Hashable (Node.ElementIndex e))

alter :: (Element e) => Alter.AlterM STM e r -> Node.ElementIndex e -> HAMT e -> STM r
alter f i v = do
  n <- readTVar v
  ((r, c), n') <- inline Node.alter (fmap exportCommand . f) (hash i) i 0 n
  case c of
    Alter.Keep -> return ()
    _ -> writeTVar v n'
  return r
  where
    exportCommand (r, c) = ((r, c), c)

insert :: (Element e) => e -> HAMT e -> STM ()
insert e = inline alter (const $ return ((), Alter.Replace e)) (Node.elementIndex e)

delete :: (Element e) => Node.ElementIndex e -> HAMT e -> STM ()
delete = inline alter (const $ return ((), Alter.Remove))

lookup :: (Element e) => Node.ElementIndex e -> HAMT e -> STM (Maybe e)
lookup = inline alter (\r -> return (r, Alter.Keep))

foldM :: (a -> e -> STM a) -> a -> HAMT e -> STM a
foldM step acc = readTVar >=> inline Node.foldM step acc 0
