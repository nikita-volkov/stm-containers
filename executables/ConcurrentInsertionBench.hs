
import STMContainers.Prelude
import Criterion.Main
import qualified Data.HashMap.Strict as UnorderedContainers
import qualified STMContainers.Map as STMContainers
import qualified Control.Concurrent.Async as Async

main =
  defaultMain
  [
    bgroup "STM Containers"
    [
      stmContainersBench 1 (max `div` 1),
      stmContainersBench 2 (max `div` 2),
      stmContainersBench 4 (max `div` 4),
      stmContainersBench 8 (max `div` 8),
      stmContainersBench 16 (max `div` 16),
      stmContainersBench 32 (max `div` 32),
      stmContainersBench 64 (max `div` 64),
      stmContainersBench 128 (max `div` 128),
      stmContainersBench 256 (max `div` 256)
    ]
    ,
    bgroup "Unordered Containers + TVar"
    [
      unorderedContainersTVarBench 1 (max `div` 1),
      unorderedContainersTVarBench 2 (max `div` 2),
      unorderedContainersTVarBench 4 (max `div` 4),
      unorderedContainersTVarBench 8 (max `div` 8),
      unorderedContainersTVarBench 16 (max `div` 16),
      unorderedContainersTVarBench 32 (max `div` 32),
      unorderedContainersTVarBench 64 (max `div` 64),
      unorderedContainersTVarBench 128 (max `div` 128),
      unorderedContainersTVarBench 256 (max `div` 256)
    ]
  ]
  where
    max = 6000

stmContainersBench :: Int -> Int -> Benchmark
stmContainersBench threads rows =
  bench (shows threads . showString "/" . shows rows $ "") $ do
    t <- atomically $ STMContainers.new :: IO (STMContainers.Map Int ())
    replicateConcurrently_ threads $ atomically $ do
      forM_ [0..rows] $ \i -> STMContainers.insert i () t

unorderedContainersTVarBench :: Int -> Int -> Benchmark
unorderedContainersTVarBench threads rows =
  bench (shows threads . showString "/" . shows rows $ "") $ do
    t <- newTVarIO UnorderedContainers.empty :: IO (TVar (UnorderedContainers.HashMap Int (TVar ())))
    replicateConcurrently_ threads $ atomically $ do
      forM_ [0..rows] $ \i -> do
        c <- newTVar ()
        tv <- readTVar t
        writeTVar t $! UnorderedContainers.insert i c tv

replicateConcurrently_ :: Int -> IO () -> IO ()
replicateConcurrently_ n io = void $ Async.mapConcurrently (const io) [1 .. n]

