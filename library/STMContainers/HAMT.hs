-- |
-- An internal node-centric API of a hash array mapped trie.
module STMContainers.HAMT where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.SizedArray as SizedArray
import qualified STMContainers.HAMT.Level as Level


-- |
-- A node with its depth level.
type NodeData e = (Level.Level, Node e)

-- |
-- An HAMT node.
type Node e = TVar (NodeState e)

data NodeState e =
  Empty |
  Nodes {-# UNPACK #-} !(WordArray.WordArray (Node e)) |
  Leaf {-# UNPACK #-} !Hash !e |
  Leaves {-# UNPACK #-} !Hash {-# UNPACK #-} !(SizedArray.SizedArray e)

type Hash = Int

class (Eq (Index e)) => Element e where
  type Index e
  elementIndex :: e -> Index e

type IndexData e = (Hash, Index e)

-- |
-- Unsafe.
-- Due to some optimizations instead of failing
-- this function might behave unpredictably,
-- when improper level is provided.
insert :: (Element e) => IndexData e -> e -> NodeData e -> STM ()
insert (h, i) e (l, n) =
  readTVar n >>= \case
    Empty -> 
      writeTVar n (Leaf h e)
    Nodes a ->
      maybe insertHere insertDeeper $ WordArray.lookup ai a
      where
        ai = Level.hashIndex l h
        insertHere = do
          n' <- newTVar (Leaf h e)
          writeTVar n (Nodes (WordArray.set ai n' a))
        insertDeeper n' = do
          insert (h, i) e (Level.succ l, n')
    Leaf h' e' ->
      if h == h'
        then if elementIndex e' == i
          then replaceWithLeaf
          else replaceWithLeaves
        else replaceWithNodes
      where
        replaceWithLeaf = 
          writeTVar n (Leaf h e)
        replaceWithLeaves =
          writeTVar n (Leaves h' (SizedArray.fromList [e, e']))
        replaceWithNodes = do
          let 
            -- Note: assuming the level doesn't overflow.
            hashIndex = Level.hashIndex l
            ai = hashIndex h
            ai' = hashIndex h'
          tv <- newTVar (Leaf h e)
          tv' <- newTVar (Leaf h' e')
          writeTVar n $ Nodes $ WordArray.fromList [(ai, tv), (ai', tv')]
    Leaves h' a' -> 
      if h == h'
        then joinTheParty
        else fork
      where
        joinTheParty =
          maybe addToOld replace $ SizedArray.map' updateAssociation a'
          where
            updateAssociation e' = 
              if elementIndex e' == i
                then Just e
                else Nothing
            replace = 
              writeTVar n . Leaves h'
            addToOld =
              writeTVar n (Leaves h' (SizedArray.append e a'))
        fork = do
          -- Note: assuming the level doesn't overflow.
          let hashIndex = Level.hashIndex l
          e1 <- (,) <$> pure (hashIndex h) <*> newTVar (Leaf h e)
          e2 <- (,) <$> pure (hashIndex h') <*> newTVar (Leaves h' a')
          writeTVar n $ Nodes $ WordArray.fromList [e1, e2]

-- | Delete and report whether the node became empty as the result.
{-# INLINE delete #-}
delete :: (Element e) => IndexData e -> NodeData e -> STM Bool
delete (hash, index) (level, node) = do
  readTVar node >>= \case
    Empty -> return True
    Nodes array ->
      case Level.hashIndex level hash of
        nodesIndex -> case WordArray.lookup nodesIndex array of
          Nothing -> return False
          Just node' -> case Level.succ level of
            level' -> do
              delete (hash, index) (level', node') >>= \case
                False -> return False
                True -> case WordArray.unset nodesIndex array of
                  array' -> case WordArray.size array' of
                    0 -> writeTVar node Empty >> return True
                    _ -> writeTVar node (Nodes array') >> return False
    Leaf hash' element' ->
      case hash' == hash of
        False -> return False
        True -> case elementIndex element' == index of
          False -> return False
          True -> writeTVar node Empty >> return True
    Leaves hash' array ->
      case hash' == hash of
        False -> return False
        True -> case SizedArray.filter ((/= index) . elementIndex) array of
          array' -> case SizedArray.size array' of
            0 -> writeTVar node Empty >> return True
            _ -> writeTVar node (Leaves hash' array') >> return False

{-# INLINE lookup #-}
lookup :: (Element e) => IndexData e -> NodeData e -> STM (Maybe e)
lookup (hash, index) (level, node) = do
  readTVar node >>= \case
    Empty -> return Nothing
    Nodes array ->
      case Level.hashIndex level hash of
        nodesIndex -> case WordArray.lookup nodesIndex array of
          Nothing -> return Nothing
          Just node' -> case Level.succ level of
            level' -> lookup (hash, index) (level', node')
    Leaf hash' element' ->
      case hash' == hash of
        True -> case elementIndex element' == index of
          True -> return (Just element')
          False -> return Nothing
        False -> return Nothing
    Leaves hash' array ->
      case hash' == hash of
        True -> case SizedArray.find ((== index) . elementIndex) array of
          Nothing -> return Nothing
          Just (_, element) -> return (Just element)
        False -> return Nothing

{-# INLINE foldM #-}
foldM :: (a -> e -> STM a) -> a -> NodeData e -> STM a
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
