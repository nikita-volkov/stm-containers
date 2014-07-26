module STMContainers.HAMT.Nodes where

import STMContainers.Prelude hiding (insert, lookup, delete, foldM, null)
import qualified STMContainers.Prelude as Prelude
import qualified STMContainers.WordArray as WordArray
import qualified STMContainers.SizedArray as SizedArray
import qualified STMContainers.HAMT.Level as Level
import qualified Focus


type Nodes e = TVar (WordArray.WordArray (Node e))

data Node e = 
  Nodes {-# UNPACK #-} !(Nodes e) |
  Leaf {-# UNPACK #-} !Hash !e |
  Leaves {-# UNPACK #-} !Hash {-# UNPACK #-} !(SizedArray.SizedArray e)

type Hash = Int

class (Eq (ElementKey e)) => Element e where
  type ElementKey e
  elementKey :: e -> ElementKey e

{-# INLINE new #-}
new :: STM (Nodes e)
new = newTVar WordArray.empty

insert :: (Element e) => e -> Hash -> ElementKey e -> Level.Level -> Nodes e -> STM ()
insert e h k l ns = do
  a <- readTVar ns
  let write n = writeTVar ns $ WordArray.set i n a
  case WordArray.lookup i a of
    Nothing -> write (Leaf h e)
    Just n -> case n of
      Nodes ns' -> insert e h k (Level.succ l) ns'
      Leaf h' e' ->
        if h' == h
          then if elementKey e' == k
            then write (Leaf h e)
            else write (Leaves h (SizedArray.pair e e'))
          else do
            nodes <- pair h (Leaf h e) h' (Leaf h' e') (Level.succ l)
            write (Nodes nodes)
      Leaves h' la ->
        if h' == h
          then case SizedArray.find ((== k) . elementKey) la of
            Just (lai, _) ->
              write (Leaves h' (SizedArray.insert lai e la))
            Nothing ->
              write (Leaves h' (SizedArray.append e la))
          else
            write . Nodes =<< pair h (Leaf h e) h' (Leaves h' la) (Level.succ l)
  where
    i = Level.hashIndex l h

pair :: Hash -> Node e -> Hash -> Node e -> Level.Level -> STM (Nodes e)
pair h1 n1 h2 n2 l =
  if i1 == i2
    then newTVar . WordArray.singleton i1 . Nodes =<< pair h1 n1 h2 n2 (Level.succ l)
    else newTVar $ WordArray.pair i1 n1 i2 n2
  where
    hashIndex = Level.hashIndex l
    i1 = hashIndex h1
    i2 = hashIndex h2

focus :: (Element e) => Focus.StrategyM STM e r -> Hash -> ElementKey e -> Level.Level -> Nodes e -> STM r
focus s h k l ns = do
  a <- readTVar ns
  (r, a'm) <- WordArray.focusM s' ai a
  maybe (return ()) (writeTVar ns) a'm
  return r
  where
    ai = Level.hashIndex l h
    s' = \case
      Nothing -> traversePair (return . fmap (Leaf h)) =<< s Nothing
      Just n -> case n of
        Nodes ns' -> do
          r <- focus s h k (Level.succ l) ns'
          null ns' >>= \case
            True -> return (r, Focus.Remove)
            False -> return (r, Focus.Keep)
        Leaf h' e' ->
          case h' == h of
            True -> 
              case elementKey e' == k of
                True  -> 
                  traversePair (return . fmap (Leaf h)) =<< s (Just e')
                False -> 
                  traversePair processDecision =<< s Nothing
                  where
                    processDecision = \case
                      Focus.Replace e -> 
                        return (Focus.Replace (Leaves h (SizedArray.pair e e')))
                      _ -> 
                        return Focus.Keep
            False -> 
              traversePair processDecision =<< s Nothing
              where
                processDecision = \case
                  Focus.Replace e -> do
                    ns' <- pair h (Leaf h e) h' (Leaf h' e') (Level.succ l)
                    return (Focus.Replace (Nodes ns'))
                  _ -> return Focus.Keep
        Leaves h' a' ->
          case h' == h of
            True ->
              case SizedArray.find ((== k) . elementKey) a' of
                Just (i', e') -> 
                  s (Just e') >>= traversePair processDecision
                  where
                    processDecision = \case
                      Focus.Keep -> 
                        return Focus.Keep
                      Focus.Remove -> 
                        case SizedArray.delete i' a' of
                          a'' -> case SizedArray.null a'' of
                            False -> return (Focus.Replace (Leaves h' a''))
                            True -> return Focus.Remove
                      Focus.Replace e ->
                        return (Focus.Replace (Leaves h' (SizedArray.insert i' e a')))
                Nothing -> 
                  s Nothing >>= traversePair processDecision
                  where
                    processDecision = \case
                      Focus.Replace e ->
                        return (Focus.Replace (Leaves h' (SizedArray.append e a')))
                      _ ->
                        return Focus.Keep
            False ->
              s Nothing >>= traversePair processDecision
              where
                processDecision = \case
                  Focus.Replace e -> do
                    ns' <- pair h (Leaf h e) h' (Leaves h' a') (Level.succ l)
                    return (Focus.Replace (Nodes ns'))
                  _ ->
                    return Focus.Keep

null :: Nodes e -> STM Bool
null = fmap WordArray.null . readTVar

foldM :: (a -> e -> STM a) -> a -> Level.Level -> Nodes e -> STM a
foldM step acc level = 
  readTVar >=> foldlM step' acc
  where
    step' acc' = \case
      Nodes ns -> foldM step acc' (Level.succ level) ns
      Leaf _ e -> step acc' e
      Leaves _ a -> SizedArray.foldM step acc' a
