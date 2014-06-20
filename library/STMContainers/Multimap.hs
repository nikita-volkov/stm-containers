module STMContainers.Multimap
(
  Multimap,
  Indexable,
  new,
  insert,
  delete,
  focus,
  lookup,
  foldM,
  null,
)
where

import STMContainers.Prelude hiding (insert, delete, lookup, alter, foldM, toList, empty, null)
import qualified Focus
import qualified STMContainers.Map as Map
import qualified STMContainers.Set as Set


-- |
newtype Multimap k v = Multimap (Map.Map k (Set.Set v))

-- |
-- A standard constraint for keys.
type Indexable a = (Eq a, Hashable a)

lookup :: (Indexable k) => k -> Multimap k v -> STM (Maybe (Set.Set v))
lookup k (Multimap m) = Map.lookup k m

insert :: (Indexable k, Indexable v) => k -> v -> Multimap k v -> STM ()
insert k v = focus (Focus.insertM v) k v

delete :: (Indexable k, Indexable v) => k -> v -> Multimap k v -> STM ()
delete k v = focus Focus.deleteM k v

focus :: (Indexable k, Indexable v) => Focus.StrategyM STM v r -> k -> v -> Multimap k v -> STM r
focus s k v (Multimap m) = 
  Map.focus ms k m
  where
    ms = \case
      Nothing -> do
        (r, d) <- s Nothing
        case d of
          Focus.Replace v' -> do
            set <- Set.new
            Set.insert v' set
            return (r, Focus.Replace set)
          _ -> return (r, Focus.Keep)
      Just set -> do
        r <- Set.focus s v set
        Set.null set >>= \case
          False -> return (r, Focus.Keep)
          True -> return (r, Focus.Remove)

foldM :: (a -> (k, v) -> STM a) -> a -> Multimap k v -> STM a
foldM f a (Multimap m) = 
  Map.foldM f' a m
  where
    f' a' (Map.Association k set) = 
      Set.foldM f'' a' set
      where
        f'' a'' v = f a'' (k, v)

new :: STM (Multimap k v)
new = Multimap <$> Map.new

null :: Multimap k v -> STM Bool
null (Multimap m) = Map.null m
