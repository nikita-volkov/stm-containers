module WordArrayTests.Update where

import Test.Framework
import STMContainers.Prelude
import MTLPrelude
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified STMContainers.WordArray as WordArray


data UpdateF e f =
  Singleton Int e f |
  Set Int e f |
  Unset Int f
  deriving (Functor)

makeFree ''UpdateF

newtype Update e r = Update (Free (UpdateF e) r)

instance Show (Update e r) where
  show = const "<Update>"

instance Arbitrary (Update Char ()) where
  arbitrary = do
    i <- bit
    v <- char
    fmap Update $ build $ singleton i v
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
      char = fmap chr $ choose (ord 'a', ord 'z')
      bit = choose (0, pred wordSize)

interpretWordArray :: Update e () -> Maybe (WordArray.WordArray e)
interpretWordArray (Update u) = 
  flip execState Nothing $ flip iterM u $ \case
    Singleton i e f -> do
      put $ Just $ WordArray.singleton i e
      f
    Set i e f -> get >>= put . fmap (WordArray.set i e) >> f
    Unset i f -> get >>= put . fmap (WordArray.unset i) >> f

interpretMaybeList :: Update e () -> Maybe [Maybe e]
interpretMaybeList (Update u) = 
  flip execState Nothing $ flip iterM u $ \case
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

