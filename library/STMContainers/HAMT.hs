-- |
-- An internal node-centric API of a hash array mapped trie.
module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.SizedArray as SizedArray
import qualified STMContainers.HAMT.Level as Level


-- |
-- A node with its depth level.
type NodeData k v = (Level, Node k v)

-- |
-- A depth level of a node.
-- Must be a multiple of the 'levelStep' value.
type Level = Int

-- |
-- An HAMT node.
type Node k v = TVar (NodeState k v)

data NodeState k v =
  Empty |
  Nodes {-# UNPACK #-} !(WordArray.WordArray (Node k v)) |
  Leaf {-# UNPACK #-} !Hash {-# UNPACK #-} !(Association k v) |
  Leaves {-# UNPACK #-} !Hash {-# UNPACK #-} !(SizedArray.SizedArray (Association k v))

-- |
-- A key-value association.
data Association k v = Association !k !v

type Hash = Int

-- |
-- An extended info on the key.
type KeyData k = (Hash, k)

type IsKey k = (Eq k, Hashable k)

-- |
-- Unsafe.
-- Due to some optimizations instead of failing
-- this function might behave unpredictably,
-- when improper level is provided.
{-# INLINE insert #-}
insert :: (IsKey k) => KeyData k -> v -> NodeData k v -> STM ()
insert (h, k) v (l, n) =
  readTVar n >>= \case
    Empty -> writeTVar n (Leaf h (Association k v))
    Nodes a ->
      maybe insertHere insertDeeper $ WordArray.lookup i a
      where
        i = Level.hashIndex l h
        insertHere = do
          n' <- newTVar (Leaf h (Association k v))
          writeTVar n (Nodes (WordArray.set i n' a))
        insertDeeper n' = do
          insert (h, k) v (Level.succ l, n')
    Leaf h' (Association k' v') ->
      if h == h'
        then if k == k'
          then replaceWithLeaf            
          else replaceWithLeaves
        else replaceWithNodes
      where
        replaceWithLeaf = writeTVar n (Leaf h (Association k v))
        replaceWithLeaves = do
          writeTVar n (Leaves h' (SizedArray.fromList [a1, a2]))
          where
            a1 = Association k v
            a2 = Association k' v'
        replaceWithNodes = do
          let 
            -- Note: assuming the level doesn't overflow.
            hashIndex = Level.hashIndex l
            i = hashIndex h
            i' = hashIndex h'
          tv <- newTVar (Leaf h (Association k v))
          tv' <- newTVar (Leaf h' (Association k' v'))
          writeTVar n $ Nodes $ WordArray.fromList [(i, tv), (i', tv')]
    Leaves h' a' -> 
      if h == h'
        then joinTheParty
        else fork
      where
        joinTheParty =
          maybe addToOld replace $ SizedArray.map' updateAssociation a'
          where
            updateAssociation (Association k' v') = 
              if k' == k 
                then Just (Association k v)
                else Nothing
            replace = 
              writeTVar n . Leaves h'
            addToOld =
              writeTVar n (Leaves h' (SizedArray.append (Association k v) a'))
        fork = do
          -- Note: assuming the level doesn't overflow.
          let hashIndex = Level.hashIndex l
          e1 <- (,) <$> pure (hashIndex h) <*> newTVar (Leaf h (Association k v))
          e2 <- (,) <$> pure (hashIndex h') <*> newTVar (Leaves h' a')
          writeTVar n $ Nodes $ WordArray.fromList [e1, e2]

-- | Delete and report whether the node became empty as the result.
{-# INLINE delete #-}
delete :: (IsKey k) => KeyData k -> NodeData k v -> STM Bool
delete (hash, key) (level, node) = do
  readTVar node >>= \case
    Empty -> return True
    Nodes array ->
      case Level.hashIndex level hash of
        index -> case WordArray.lookup index array of
          Nothing -> return False
          Just node' -> case Level.succ level of
            level' -> do
              delete (hash, key) (level', node') >>= \case
                False -> return False
                True -> case WordArray.unset index array of
                  array' -> case WordArray.size array' of
                    0 -> writeTVar node Empty >> return True
                    _ -> writeTVar node (Nodes array') >> return False
    Leaf hash' (Association key' _) ->
      case hash' == hash of
        False -> return False
        True -> case key' == key of
          False -> return False
          True -> writeTVar node Empty >> return True
    Leaves hash' array ->
      case hash' == hash of
        False -> return False
        True -> case SizedArray.filter (\(Association key' _) -> key' /= key) array of
          array' -> case SizedArray.size array' of
            0 -> writeTVar node Empty >> return True
            _ -> writeTVar node (Leaves hash' array') >> return False

{-# INLINE lookup #-}
lookup :: (IsKey k) => KeyData k -> NodeData k v -> STM (Maybe v)
lookup (hash, key) (level, node) = do
  readTVar node >>= \case
    Empty -> return Nothing
    Nodes array ->
      case Level.hashIndex level hash of
        index -> case WordArray.lookup index array of
          Nothing -> return Nothing
          Just node' -> case Level.succ level of
            level' -> lookup (hash, key) (level', node')
    Leaf hash' (Association key' value) ->
      case hash' == hash of
        True -> case key' == key of
          True -> return (Just value)
          False -> return Nothing
        False -> return Nothing
    Leaves hash' array ->
      case hash' == hash of
        True -> case SizedArray.find (\(Association key' _) -> key' == key) array of
          Nothing -> return Nothing
          Just (_, (Association _ value)) -> return (Just value)
        False -> return Nothing

{-# INLINE foldM #-}
foldM :: (a -> Association k v -> STM a) -> a -> NodeData k v -> STM a
foldM step acc (level, node) = 
  readTVar node >>= \case
    Empty -> 
      return acc
    Nodes array ->
      WordArray.foldM step' acc array
      where
        step' acc' node' = foldM step acc' (Level.succ level, node')
    Leaf hash' association ->
      step acc association
    Leaves hash' array ->
      SizedArray.foldM step acc array
