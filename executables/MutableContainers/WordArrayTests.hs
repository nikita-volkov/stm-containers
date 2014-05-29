{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MutableContainers.WordArrayTests where

import Test.Framework
import Test.QuickCheck.Monadic
import MutableContainers.Prelude
import MutableContainers.Transformers
import Control.Monad.Trans.Free
import Control.Monad.Free.TH
import qualified MutableContainers.WordArray as WordArray
import qualified Data.Char as Char


data UpdateF e f =
  Singleton Int e f |
  Set Int e f |
  Unset Int f
  deriving (Functor)

type Update e = FreeT (UpdateF e)

makeFree ''UpdateF


interpretUpdateWordArray :: 
  WordArray.WordArrayMonad m => 
  Update e m () -> m (Maybe (WordArray.WordArray m e))
interpretUpdateWordArray u = 
  flip execStateT Nothing $ flip iterTM u $ \case
    Singleton i e f -> lift (WordArray.singleton i e) >>= put . Just >> f
    Set i e f -> get >>= lift . mapM (\c -> WordArray.set c i e) >> f
    Unset i f -> get >>= lift . mapM (\c -> WordArray.unset c i) >> f

interpretUpdateMaybeList :: Monad m => Update e m () -> m (Maybe [Maybe e])
interpretUpdateMaybeList u = 
  flip execStateT Nothing $ flip iterTM u $ \case
    Singleton i e f -> do
      put $ Just $ map (\i' -> if i == i' then Just e else Nothing) [0 .. pred wordSize]
      f
    Set i e f -> do
      lm <- get
      lm' <- forM lm $ \l -> 
        return $ do
          (i', e') <- zip [0..] l
          return $ if i == i' then Just e else e'
      put lm'
      f
    Unset i f -> do
      lm <- get
      put $ flip fmap lm $ \l -> do
        (i', e') <- zip [0..] l
        return $ if i == i' then Nothing else e'
      f

wordSize :: Int
wordSize = bitSize (undefined :: Word)


instance (Monad m) => Arbitrary (Update Char m ()) where
  arbitrary = do
    i <- bit
    v <- char
    build $ singleton i v
    where
      build = execStateT $ do
        amount <- lift $ choose (0, 200)
        replicateM_ amount $ do
          lift (choose (0 :: Int, 1)) >>= \case
            0 -> do
              i <- lift $ bit
              v <- lift $ char
              modify (>> set i v)
            1 -> do
              i <- lift $ bit
              modify (>> unset i)
      char = fmap Char.chr $ choose (Char.ord 'a', Char.ord 'z')
      bit = choose (0, pred wordSize)


-- * Tests
-------------------------

prop_differentInterpretersProduceSameResultsForIO =
  monadicIO $ do
    update :: Update Char IO () <- run $ generate arbitrary
    l <- run $ interpretUpdateMaybeList update
    w <- run $ interpretUpdateWordArray update >>= mapM WordArray.toList
    assert $ w == l

prop_differentInterpretersProduceSameResultsForSTM =
  monadicIO $ do
    update :: Update Char STM () <- run $ generate arbitrary
    l <- run $ atomically $ interpretUpdateMaybeList update
    w <- run $ atomically $ interpretUpdateWordArray update >>= mapM WordArray.toList
    assert $ w == l

