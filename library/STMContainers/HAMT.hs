module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.HAMT.Nodes as Nodes
import qualified Focus


type HAMT e = Nodes.Nodes e

type Element e = (Nodes.Element e, Hashable (Nodes.ElementKey e))

{-# INLINE insert #-}
insert :: (Element e) => e -> HAMT e -> STM ()
insert e = Nodes.insert e (hash (Nodes.elementKey e)) (Nodes.elementKey e) 0

{-# INLINE focus #-}
focus :: (Element e) => Focus.StrategyM STM e r -> Nodes.ElementKey e -> HAMT e -> STM r
focus s k = Nodes.focus s (hash k) k 0

{-# INLINE foldM #-}
foldM :: (a -> e -> STM a) -> a -> HAMT e -> STM a
foldM step acc = Nodes.foldM step acc 0

{-# INLINE new #-}
new :: STM (HAMT e)
new = Nodes.new

{-# INLINE null #-}
null :: HAMT e -> STM Bool
null v = $notImplemented
