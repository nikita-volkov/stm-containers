module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.HAMT.Node as Node
import qualified STMContainers.Visit as Visit


type HAMT e = TVar (Node.Node e)

type Element e = (Node.Element e, Hashable (Node.ElementIndex e))

visit :: (Element e) => Visit.VisitM STM e r -> Node.ElementIndex e -> HAMT e -> STM r
visit f i v = do
  n <- readTVar v
  ((r, c), n') <- inline Node.visit (fmap exportCommand . f) (hash i) i 0 n
  case c of
    Visit.Keep -> return ()
    _ -> writeTVar v n'
  return r
  where
    exportCommand (r, c) = ((r, c), c)

insert :: (Element e) => e -> HAMT e -> STM ()
insert e = inline visit ((Visit.monadize . Visit.insert) e) (Node.elementIndex e)

delete :: (Element e) => Node.ElementIndex e -> HAMT e -> STM ()
delete = inline visit (Visit.monadize Visit.delete)

lookup :: (Element e) => Node.ElementIndex e -> HAMT e -> STM (Maybe e)
lookup = inline visit (Visit.monadize Visit.lookup)

foldM :: (a -> e -> STM a) -> a -> HAMT e -> STM a
foldM step acc = readTVar >=> inline Node.foldM step acc 0

new :: STM (HAMT e)
new = newTVar Node.Empty
