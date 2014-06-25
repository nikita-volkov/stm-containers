
import STMContainers.Prelude
import Criterion.Main
import qualified Data.HashMap.Strict as UnorderedContainers
import qualified STMContainers.Map as STMContainers
import qualified Control.Concurrent.Async as Async


main =
  defaultMain
  [
    threadsRowsGroup 1 (max `div` 1),
    threadsRowsGroup 3 (max `div` 3),
    threadsRowsGroup 6 (max `div` 6),
    threadsRowsGroup 12 (max `div` 12),
    threadsRowsGroup 24 (max `div` 24)
  ]
  where
    max = 30000

threadsRowsGroup :: Int -> Int -> Benchmark
threadsRowsGroup threads rows =
  bgroup 
  (shows threads . showString "/" . shows rows $ "")
  [
    stmContainersBench threads rows,
    unorderedContainersTVarBench threads rows
  ]

stmContainersBench :: Int -> Int -> Benchmark
stmContainersBench threads rows =
  bench ("STM Containers") $ do
    t <- atomically $ STMContainers.new :: IO (STMContainers.Map Int ())
    flip Async.mapConcurrently [0 .. pred threads] $ \ti -> do
      forM_ [rows * ti .. (pred rows) * ti] $ \ri -> do
        atomically $ STMContainers.insert ri () t

unorderedContainersTVarBench :: Int -> Int -> Benchmark
unorderedContainersTVarBench threads rows =
  bench ("Unordered Containers + TVar") $ do
    t <- newTVarIO UnorderedContainers.empty :: IO (TVar (UnorderedContainers.HashMap Int (TVar ())))
    flip Async.mapConcurrently [0 .. pred threads] $ \ti -> do
      forM_ [rows * ti .. (pred rows) * ti] $ \ri -> 
        atomically $ do
          c <- newTVar ()
          tv <- readTVar t
          writeTVar t $! UnorderedContainers.insert ri c tv
