-- |
-- An internal node-centric API of a hash array mapped trie.
module STMContainers.HashTable.Node where

import STMContainers.Prelude hiding (insert, lookup, delete)
import Data.Primitive.Array
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.SizedArray as SizedArray


-- |
-- A node with its level.
type NodeData k v = (Level, Node k v)

-- |
-- A depth level of this node.
-- A multiple of 'levelStep' value.
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
insert :: (IsKey k) => NodeData k v -> KeyData k -> v -> STM ()
insert (l, n) (h, k) v =
  readTVar n >>= \case
    Empty -> writeTVar n (Leaf h (Association k v))
    Nodes a ->
      maybe insertHere insertDeeper $ WordArray.lookup i a
      where
        i = levelHashIndex l h
        insertHere = do
          n' <- newTVar (Leaf h (Association k v))
          writeTVar n (Nodes (WordArray.set i n' a))
        insertDeeper n' = do
          insert (l + levelStep, n') (h, k) v
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
            hashIndex = levelHashIndex l
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
          let hashIndex = levelHashIndex l
          e1 <- (,) <$> pure (hashIndex h) <*> newTVar (Leaf h (Association k v))
          e2 <- (,) <$> pure (hashIndex h') <*> newTVar (Leaves h' a')
          writeTVar n $ Nodes $ WordArray.fromList [e1, e2]

-- | Delete and report whether the node became empty.
{-# INLINE delete #-}
delete :: (IsKey k) => NodeData k v -> KeyData k -> STM Bool
delete (level, node) (hash, key) = do
  readTVar node >>= \case
    Empty -> return True
    Nodes array ->
      case levelHashIndex level hash of
        index -> case WordArray.lookup index array of
          Nothing -> return False
          Just node' -> case level + levelStep of
            level' -> do
              delete (level', node') (hash, key) >>= \case
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




levelHashIndex :: Level -> (Hash -> Int)
levelHashIndex l = mask . shift
  where
    mask = (levelMask .&.)
    shift i = unsafeShiftR i l

levelMask :: Int
levelMask = 2 ^ levelStep - 1

levelStep :: Int
levelStep = 5

levelLimit :: Int
levelLimit = bitSize (undefined :: Int)
