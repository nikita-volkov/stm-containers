
import STMContainers.Prelude
import Criterion.Main
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified Data.HashMap.Strict as UC
import qualified STMContainers.Map as SC
import qualified Control.Concurrent.Async as Async
import qualified System.Random.MWC.Monad as MWC
import qualified Focus


type UCMap k v = TVar (UC.HashMap k (TVar v))


-- * Actions 
-------------------------

data ActionF k v n where
  Insert :: k -> v -> n -> ActionF k v n
  deriving (Functor)

type Action k v = Free (ActionF k v)


-- * Interpreters
-------------------------

type Interpreter m = 
  forall k v r. (Hashable k, Eq k) => m k v -> Action k v r -> STM r

ucInterpreter :: Interpreter UCMap
ucInterpreter m = 
  iterM $ \case
    Insert k v n -> do
      mv <- readTVar m
      vt <- newTVar v
      writeTVar m $! UC.insert k vt mv
      n

specializedSCInterpreter :: Interpreter SC.Map
specializedSCInterpreter m =
  iterM $ \case
    Insert k v n -> SC.insert k v m >> n

focusSCInterpreter :: Interpreter SC.Map
focusSCInterpreter m =
  iterM $ \case
    Insert k v n -> SC.focus (Focus.insertM v) k m >> n


-- * Session and runners
-------------------------

-- | A list of transactions per thread.
type Session k v = [[Action k v ()]]

type SessionRunner = 
  forall k v. (Hashable k, Eq k) => Session k v -> IO ()

scSessionRunner :: Interpreter SC.Map -> SessionRunner
scSessionRunner interpreter threadActions = do
  m <- atomically $ SC.new
  void $ flip Async.mapConcurrently threadActions $ \actions -> do
    forM_ actions $ atomically . interpreter m

ucSessionRunner :: SessionRunner
ucSessionRunner threadActions = do
  m <- newTVarIO UC.empty
  void $ flip Async.mapConcurrently threadActions $ \actions -> do
    forM_ actions $ atomically . ucInterpreter m


-- * Generators
-------------------------

type Generator a = MWC.Rand IO a

transactionGenerator :: Generator (Action Float Int ())
transactionGenerator =
  Free <$> (Insert <$> k <*> v <*> n)
  where
    k = MWC.uniform
    v = MWC.uniform
    n = MWC.uniform >>= bool (return (Pure ())) transactionGenerator


-- * Utils
-------------------------

slices :: Int -> [a] -> [[a]]
slices size l =
  case splitAt size l of
    ([], _) -> []
    (a, b) -> a : slices size b


-- * Main
-------------------------

main = do
  allActions <- MWC.runWithCreate $ replicateM actionsNum transactionGenerator
  defaultMain $! flip map threadsNums $! \threadsNum ->
    let
      sliceSize = actionsNum `div` threadsNum
      threadActions = slices sliceSize allActions
      in 
        bgroup
          (shows threadsNum . showString "/" . shows sliceSize $ "")
          [
            bgroup "STM Containers"
              [
                bench "Focus-based" $ 
                  scSessionRunner focusSCInterpreter threadActions,
                bench "Specialized" $ 
                  scSessionRunner specializedSCInterpreter threadActions
              ],
            bench "Unordered Containers" $
              ucSessionRunner threadActions
          ]
  where
    actionsNum = 100000
    threadsNums = [1, 2, 4, 8, 16, 32]
