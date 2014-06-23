
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
      stmContainersBench 1 10000,
      stmContainersBench 2 5000,
      stmContainersBench 4 2500,
      stmContainersBench 8 1250
    ]
    ,
    bgroup "Unordered Containers + TVar"
    [
      unorderedContainersTVarBench 1 10000,
      unorderedContainersTVarBench 2 5000,
      unorderedContainersTVarBench 4 2500,
      unorderedContainersTVarBench 8 1250
    ]
  ]

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

