
import STMContainers.Prelude
import Criterion.Main
import qualified Data.HashTable.IO as Hashtables
import qualified Data.HashMap.Strict as UnorderedContainers
import qualified Data.Map as Containers
import qualified STMContainers.Map as STMContainers

main =
  defaultMain
  [
    bgroup "STM Containers"
    [
      stmContainersBench 10000
    ]
    ,
    bgroup "Unordered Containers + TVar"
    [
      unorderedContainersTVarBench 10000
    ]
    ,
    bgroup "Hashtables"
    [
      hashtablesBench 10000
    ]
    ,
    bgroup "Unordered Containers"
    [
      unorderedContainersBench 10000
    ]
    ,
    bgroup "Containers"
    [
      containersBench 10000
    ]
  ]

stmContainersBench :: Int -> Benchmark
stmContainersBench rows =
  bench (show rows) $ atomically $ do
    t <- STMContainers.new :: STM (STMContainers.Map Int ())
    forM_ [0..rows] $ \i -> STMContainers.insert i () t

unorderedContainersTVarBench :: Int -> Benchmark
unorderedContainersTVarBench rows =
  bench (show rows) $ atomically $ do
    t <- newTVar UnorderedContainers.empty :: STM (TVar (UnorderedContainers.HashMap Int (TVar ())))
    forM_ [0..rows] $ \i -> do
      c <- newTVar ()
      tv <- readTVar t
      writeTVar t $! UnorderedContainers.insert i c tv

hashtablesBench :: Int -> Benchmark
hashtablesBench rows =
  bench (show rows) $ do
    t <- Hashtables.new :: IO (Hashtables.BasicHashTable Int ())
    forM_ [0..rows] $ \i -> Hashtables.insert t i ()

unorderedContainersBench :: Int -> Benchmark
unorderedContainersBench rows =
  bench (show rows) $ 
    nf (foldr (\k -> UnorderedContainers.insert k ()) UnorderedContainers.empty) [0..rows]

containersBench :: Int -> Benchmark
containersBench rows =
  bench (show rows) $ 
    nf (foldr (\k -> Containers.insert k ()) Containers.empty) [0..rows]
