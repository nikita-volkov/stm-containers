-- |
-- An internal node-centric API of a hash array mapped trie.
module STMContainers.HAMT.Node where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM)
import qualified STMContainers.SizedArray as SizedArray
import qualified STMContainers.Visit as Visit
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
visit :: (Element e) => Visit.VisitM STM e r -> Hash -> ElementIndex e -> Level.Level -> Node e -> STM (r, Node e)
visit f h i l = \case
  Empty -> 
    fmap commandToNode <$> f Nothing
    where
      commandToNode = \case
        Visit.Replace e -> Leaf h e
        _ -> Empty
  Nodes nodes ->
    fmap nodesToNode <$> inline Nodes.visit f' (Level.hashIndex l h) nodes
    where
      f' = \case
        Just n -> 
          fmap nodeToCommand <$> visit f h i (Level.succ l) n
          where
            nodeToCommand = \case
              Empty -> Visit.Remove
              n' -> Visit.Replace n'
        Nothing -> 
          (fmap . fmap) (Leaf h) <$> f Nothing
      nodesToNode nodes' = case Nodes.null nodes' of 
        True -> Empty
        False -> Nodes nodes'
  Leaf h' e' ->
    case h == h' of
      True -> case elementIndex e' == i of
        True -> 
          fmap commandToNode <$> f (Just e')
          where
            commandToNode = \case
              Visit.Keep -> Leaf h' e'
              Visit.Remove -> Empty
              Visit.Replace e -> Leaf h e
        False ->
          fmap commandToNode <$> f Nothing
          where
            commandToNode = \case
              Visit.Replace e -> Leaves h (SizedArray.pair e e')
              _ -> Leaf h' e'
      False ->
        mapM commandToNodeM =<< f Nothing
        where
          commandToNodeM = \case
            Visit.Replace e -> pair h (Leaf h e) h' (Leaf h' e') l
            _ -> return (Leaf h' e')
  Leaves h' a' ->
    case h == h' of
      True -> 
        case SizedArray.find ((== i) . elementIndex) a' of
          Just (ai', e') -> fmap commandToNode <$> f (Just e')
            where
              commandToNode = \case
                Visit.Keep -> 
                  Leaves h' a'
                Visit.Remove ->
                  case SizedArray.delete ai' a' of
                    a'' -> case SizedArray.null a'' of
                      True -> Empty
                      False -> Leaves h' a''
                Visit.Replace e -> 
                  Leaves h' (SizedArray.insert ai' e a')
          Nothing -> fmap commandToNode <$> f Nothing
            where
              commandToNode = \case
                Visit.Replace e ->
                  Leaves h' (SizedArray.append e a')
                _ ->
                  Leaves h' a'
      False ->
        mapM commandToNodeM =<< f Nothing
        where
          commandToNodeM = \case
            Visit.Replace e -> pair h (Leaf h e) h' (Leaves h' a') l
            _ -> return (Leaves h' a')

-- |
-- Assumes that the hashes aren't equal.
pair :: Hash -> Node e -> Hash -> Node e -> Level.Level -> STM (Node e)
pair h1 n1 h2 n2 l =
  case i1 == i2 of
    True -> return . Nodes =<< Nodes.singleton i1 =<< pair h1 n1 h2 n2 (Level.succ l)
    False -> Nodes <$> Nodes.pair i1 n1 i2 n2
  where
    hashIndex = Level.hashIndex l
    i1 = hashIndex h1
    i2 = hashIndex h2

foldM :: (a -> e -> STM a) -> a -> Level.Level -> Node e -> STM a
foldM step acc level = \case
  Empty -> 
    return acc
  Nodes array ->
    inline Nodes.foldM step' acc array
    where
      step' acc' = foldM step acc' (Level.succ level)
  Leaf _ element ->
    step acc element
  Leaves _ array ->
    inline SizedArray.foldM step acc array
