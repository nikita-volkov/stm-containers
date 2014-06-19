-- |
-- An internal node-centric API of a hash array mapped trie.
module STMContainers.HAMT.Node where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.SizedArray as SizedArray
import qualified STMContainers.Alter as Alter
import qualified STMContainers.HAMT.Node.Level as Level
import qualified STMContainers.HAMT.Node.Nodes as Nodes


data Node e =
  Empty |
  Nodes {-# UNPACK #-} !(Nodes.Nodes (Node e)) |
  Leaf {-# UNPACK #-} !Hash !e |
  Leaves {-# UNPACK #-} !Hash {-# UNPACK #-} !(SizedArray.SizedArray e)

type Hash = Int

class (Eq (ElementIndex e)) => Element e where
  type ElementIndex e
  elementIndex :: e -> ElementIndex e

-- |
-- Unsafe.
-- Due to some optimizations instead of failing
-- this function might behave unpredictably,
-- when improper level is provided.
alter :: (Element e) => Alter.AlterM STM e r -> Hash -> ElementIndex e -> Level.Level -> Node e -> STM (r, Node e)
alter f h i l = \case
  Empty -> 
    fmap commandToNode <$> f Nothing
    where
      commandToNode = \case
        Alter.Replace e -> Leaf h e
        _ -> Empty
  Nodes nodes ->
    fmap nodesToNode <$> inline Nodes.alterM f' (Level.hashIndex l h) nodes
    where
      f' = \case
        Just n -> 
          fmap nodeToCommand <$> alter f h i (Level.succ l) n
          where
            nodeToCommand = \case
              Empty -> Alter.Remove
              n' -> Alter.Replace n'
        Nothing -> 
          (fmap . fmap) (Leaf h) <$> f Nothing
      nodesToNode nodes' = case Nodes.null nodes' of 
        True -> Empty
        False -> Nodes nodes'
  Leaf h' e ->
    case h == h' of
      True -> case elementIndex e == i of
        True -> 
          fmap commandToNode <$> f (Just e)
          where
            commandToNode = \case
              Alter.Keep -> Leaf h e
              Alter.Remove -> Empty
              Alter.Replace e' -> Leaf h e'
        False ->
          fmap commandToNode <$> f Nothing
          where
            commandToNode = \case
              Alter.Replace e' -> Leaves h (SizedArray.fromList [e, e'])
              _ -> Leaf h' e
      False ->
        mapM commandToNodeM =<< f Nothing
        where
          commandToNodeM = \case
            Alter.Replace e' -> 
              Nodes <$> Nodes.fromSizedList (2, [(ni, n), (ni', n')])
              where
                -- Note: assuming the level doesn't overflow.
                hashIndex = Level.hashIndex l
                ni = hashIndex h
                ni' = hashIndex h'
                n = Leaf h e
                n' = Leaf h' e'
            _ -> return (Leaf h' e)
  Leaves h' a ->
    case h == h' of
      False ->
        mapM commandToNodeM =<< f Nothing
        where
          commandToNodeM = \case
            Alter.Replace e' -> 
              Nodes <$> Nodes.fromSizedList (2, [(ni, n), (ni', n')])
              where
                -- Note: assuming the level doesn't overflow.
                hashIndex = Level.hashIndex l
                ni = hashIndex h
                ni' = hashIndex h'
                n = Leaf h e'
                n' = Leaves h' a
            _ -> return (Leaves h' a)
      True -> 
        case SizedArray.find ((== i) . elementIndex) a of
          Just (ai, e) -> fmap commandToNode <$> f (Just e)
            where
              commandToNode = \case
                Alter.Keep -> 
                  Leaves h' a
                Alter.Remove ->
                  case SizedArray.delete ai a of
                    a' -> case SizedArray.null a' of
                      True -> Empty
                      False -> Leaves h' a'
                Alter.Replace e' -> 
                  Leaves h' (SizedArray.insert ai e' a)
          Nothing -> fmap commandToNode <$> f Nothing
            where
              commandToNode = \case
                Alter.Replace e' ->
                  Leaves h' (SizedArray.append e' a)
                _ ->
                  Leaves h' a

foldM :: (a -> e -> STM a) -> a -> Level.Level -> Node e -> STM a
foldM step acc level = \case
  Empty -> 
    return acc
  Nodes array ->
    inline Nodes.foldM step' acc array
    where
      step' acc' = foldM step acc' (Level.succ level)
  Leaf hash' element ->
    step acc element
  Leaves hash' array ->
    inline SizedArray.foldM step acc array
