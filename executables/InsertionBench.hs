
import STMContainers.Prelude
import Criterion.Main
import qualified Data.HashTable.IO as Hashtables
import qualified Data.HashMap.Strict as UnorderedContainers
import qualified Data.Map as Containers
import qualified STMContainers.Map as STMContainers
import qualified Focus
import qualified System.Random as Random

main =
  defaultMain
  [
    bgroup "STM Containers"
    [
      bench "focus-based" $ 
        do
          t <- atomically $ STMContainers.new :: IO (STMContainers.Map String ())
          forM_ [0..rows] $ \i -> atomically $ STMContainers.focus (Focus.insertM ()) (iToK i) t
      ,
      bench "specialized" $
        do
          t <- atomically $ STMContainers.new :: IO (STMContainers.Map String ())
          forM_ [0..rows] $ \i -> atomically $ STMContainers.insert (iToK i) () t
    ]
    ,
    bench "Unordered Containers + TVar" $
      do
        t <- newTVarIO UnorderedContainers.empty :: IO (TVar (UnorderedContainers.HashMap String (TVar ())))
        forM_ [0..rows] $ \i -> atomically $ do
          c <- newTVar ()
          tv <- readTVar t
          writeTVar t $! UnorderedContainers.insert (iToK i) c tv
    ,
    bench "Unordered Containers" $
      nf (foldr (\i -> UnorderedContainers.insert (iToK i) ()) UnorderedContainers.empty) [0..rows]
    ,
    bench "Containers" $
      nf (foldr (\i -> Containers.insert (iToK i) ()) Containers.empty) [0..rows]
    ,
    bench "Hashtables" $ 
      do
        t <- Hashtables.new :: IO (Hashtables.BasicHashTable String ())
        forM_ [0..rows] $ \i -> Hashtables.insert t (iToK i) ()
  ]

rows :: Int = 100000

-- | Produce a pseudo-random key.
iToK :: Int -> String
iToK i = case Random.mkStdGen i of g -> take 7 $ Random.randomRs ('a', 'z') g
