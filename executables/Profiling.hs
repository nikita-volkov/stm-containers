
import STMContainers.Prelude
import qualified STMContainers.Map as STMContainers
import qualified Control.Concurrent.Async as Async


main = do
  t <- atomically $ STMContainers.new :: IO (STMContainers.Map Int ())
  void $ flip Async.mapConcurrently [0 .. pred threads] $ \ti -> do
    forM_ [ti * (rows `div` threads) .. (succ ti) * (rows `div` threads)] $ \ri -> do
      atomically $ STMContainers.insert () ri t
  where
    threads = 4
    rows = 100000
