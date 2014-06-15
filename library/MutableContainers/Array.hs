module MutableContainers.Array where

import MutableContainers.Prelude
import qualified Data.Primitive.Array as Primitive
import qualified Control.Foldl as Foldl


class Monad m => ArrayMonad m where
  data Array m a
  new :: Int -> a -> m (Array m a)
  read :: Array m a -> Int -> m a
  write :: Array m a -> Int -> a -> m ()
  indexM :: Primitive.Array a -> Int -> m a
  unsafeFreeze :: Array m a -> m (Primitive.Array a)
  unsafeThaw :: Primitive.Array a -> m (Array m a)
  fromList :: [a] -> m (Array m a)

type Index = Int

instance ArrayMonad STM where
  newtype Array STM e = STMArray (Primitive.Array (TVar e))
  new size e = $notImplemented
  fromList list = do
    (tvars, size) <-
      let 
        fold = (,) <$> tvars <*> size
          where
            tvars = 
              Foldl.FoldM step (return []) return
              where
                step r a = fmap (:r) (newTVar a)
            size =
              Foldl.generalize Foldl.length
        in Foldl.foldM fold list
    let array = 
          runST $ do
            array <- Primitive.newArray size undefined
            forM_ (zip tvars [0..]) $ \(tvar, i) -> Primitive.writeArray array i tvar
            Primitive.unsafeFreezeArray array
    return $ STMArray array


instance ArrayMonad (ST s) where
  newtype Array (ST s) e = STArray (Primitive.MutableArray s e)

instance ArrayMonad IO where
  newtype Array IO e = IOArray (Primitive.MutableArray RealWorld e)

