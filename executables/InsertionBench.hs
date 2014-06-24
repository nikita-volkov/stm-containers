
import STMContainers.Prelude
import Criterion.Main
import qualified Data.HashTable.IO as Hashtables
import qualified Data.HashMap.Strict as UnorderedContainers
import qualified Data.Map as Containers
import qualified STMContainers.Map as STMContainers
import qualified Focus

main =
  defaultMain
  [
    bgroup "STM Containers"
    [
      bench "insert" $
        atomically $ do
          t <- STMContainers.new :: STM (STMContainers.Map Int ())
          forM_ [0..rows] $ \i -> STMContainers.insert i () t
      ,
      bench "focus-insert" $ 
        atomically $ do
          t <- STMContainers.new :: STM (STMContainers.Map Int ())
          forM_ [0..rows] $ \i -> STMContainers.focus (Focus.insertM ()) i t
    ]
    ,
    bench "Unordered Containers + TVar" $
      atomically $ do
        t <- newTVar UnorderedContainers.empty :: STM (TVar (UnorderedContainers.HashMap Int (TVar ())))
        forM_ [0..rows] $ \i -> do
          c <- newTVar ()
          tv <- readTVar t
          writeTVar t $! UnorderedContainers.insert i c tv
    ,
    bench "Hashtables" $ 
      do
        t <- Hashtables.new :: IO (Hashtables.BasicHashTable Int ())
        forM_ [0..rows] $ \i -> Hashtables.insert t i ()
    ,
    bench "Unordered Containers" $
      nf (foldr (\k -> UnorderedContainers.insert k ()) UnorderedContainers.empty) [0..rows]
    ,
    bench "Containers" $
      nf (foldr (\k -> Containers.insert k ()) Containers.empty) [0..rows]
  ]

rows :: Int = 10000
