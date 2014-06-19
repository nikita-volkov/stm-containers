{-# OPTIONS_GHC -F -pgmF htfpp #-}
module APITests.MapTests where

import Test.Framework
import STMContainers.Prelude
import STMContainers.Transformers
import Control.Monad.Free
import qualified APITests.MapTests.Update as Update
import qualified STMContainers.Map as STMMap
import qualified STMContainers.Visit as Visit
import qualified Data.HashMap.Strict as HashMap


interpretSTMMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> STM (STMMap.Map k v)
interpretSTMMapUpdate update = do
  m <- STMMap.new
  flip iterM update $ \case
    Update.Insert k v c -> STMMap.insert k v m >> c
    Update.Delete k c   -> STMMap.delete k m >> c
    Update.Update f k c -> STMMap.alter ((Visit.monadize . Visit.update) f) k m >> c
  return m

interpretHashMapUpdate :: (Hashable k, Eq k) => Update.Update k v -> HashMap.HashMap k v
interpretHashMapUpdate update = 
  flip execState HashMap.empty $ flip iterM update $ \case
    Update.Insert k v c -> modify (HashMap.insert k v) >> c
    Update.Delete k c   -> modify (HashMap.delete k) >> c
    Update.Update f k c -> modify (HashMap.insert k (f undefined)) >> c

stmMapToHashMap :: (Hashable k, Eq k) => STMMap.Map k v -> STM (HashMap.HashMap k v)
stmMapToHashMap = STMMap.foldM f HashMap.empty
  where
    f m (STMMap.Association k v) = return (HashMap.insert k v m)


-- * Tests
-------------------------

prop_updatesProduceTheSameEffectAsInHashMap (updates :: [Update.Update Word8 Char]) =
  trace ("---\n" ++ show b ++ "\n" ++ show a) $
  a == b
  where
    update = sequence_ updates
    a = interpretHashMapUpdate update
    b = (unsafePerformIO . atomically . (stmMapToHashMap <=< interpretSTMMapUpdate)) update


