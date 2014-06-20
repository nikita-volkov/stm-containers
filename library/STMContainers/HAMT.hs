module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.HAMT.Node as Node
import qualified Focus


type HAMT e = TVar (Node.Node e)

type Element e = (Node.Element e, Hashable (Node.ElementIndex e))

focus :: (Element e) => Focus.StrategyM STM e r -> Node.ElementIndex e -> HAMT e -> STM r
focus f i v = do
  n <- readTVar v
  ((r, c), n') <- inline Node.focus (fmap exportCommand . f) (hash i) i 0 n
  case c of
    Focus.Keep -> return ()
    _ -> writeTVar v n'
  return r
  where
    exportCommand (r, c) = ((r, c), c)

insert :: (Element e) => e -> HAMT e -> STM ()
insert e = inline focus (Focus.insertM e) (Node.elementIndex e)

delete :: (Element e) => Node.ElementIndex e -> HAMT e -> STM ()
delete = inline focus Focus.deleteM

lookup :: (Element e) => Node.ElementIndex e -> HAMT e -> STM (Maybe e)
lookup = inline focus Focus.lookupM

foldM :: (a -> e -> STM a) -> a -> HAMT e -> STM a
foldM step acc = readTVar >=> inline Node.foldM step acc 0

new :: STM (HAMT e)
new = newTVar Node.Empty

null :: HAMT e -> STM Bool
null v = readTVar v >>= \case
  Node.Empty -> return True
  _ -> return False
