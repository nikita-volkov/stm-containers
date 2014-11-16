
import BasePrelude
import Data.Hashable (Hashable)
import Criterion.Main
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified Data.HashMap.Strict as UC
import qualified STMContainers.Map as SC
import qualified Control.Concurrent.Async as Async
import qualified System.Random.MWC.Monad as MWC
import qualified Focus
import qualified Data.Char as Char
import qualified Data.Text as Text


type UCMap k v = TVar (UC.HashMap k (TVar v))


-- * Transactions 
-------------------------

data TransactionF k v n where
  Insert :: v -> k -> n -> TransactionF k v n
  deriving (Functor)

type Transaction k v = Free (TransactionF k v)


-- * Interpreters
-------------------------

type Interpreter m = 
  forall k v r. (Hashable k, Eq k) => m k v -> Transaction k v r -> STM r

ucInterpreter :: Interpreter UCMap
ucInterpreter m = 
  iterM $ \case
    Insert v k n -> do
      mv <- readTVar m
      vt <- newTVar v
      writeTVar m $! UC.insert k vt mv
      n

specializedSCInterpreter :: Interpreter SC.Map
specializedSCInterpreter m =
  iterM $ \case
    Insert v k n -> SC.insert v k m >> n

focusSCInterpreter :: Interpreter SC.Map
focusSCInterpreter m =
  iterM $ \case
    Insert v k n -> SC.focus (Focus.insertM v) k m >> n


-- * Session and runners
-------------------------

-- | A list of transactions per thread.
type Session k v = [[Transaction k v ()]]

type SessionRunner = 
  forall k v. (Hashable k, Eq k) => Session k v -> IO ()

scSessionRunner :: Interpreter SC.Map -> SessionRunner
scSessionRunner interpreter threadTransactions = do
  m <- atomically $ SC.new
  void $ flip Async.mapConcurrently threadTransactions $ \actions -> do
    forM_ actions $ atomically . interpreter m

ucSessionRunner :: SessionRunner
ucSessionRunner threadTransactions = do
  m <- newTVarIO UC.empty
  void $ flip Async.mapConcurrently threadTransactions $ \actions -> do
    forM_ actions $ atomically . ucInterpreter m


-- * Generators
-------------------------

type Generator a = MWC.Rand IO a

transactionGenerator :: Generator (Transaction Text.Text () ())
transactionGenerator = do
  k <- keyGenerator
  return $ Free $ Insert () k (Pure ())

keyGenerator :: Generator Text.Text
keyGenerator = do
  l <- length
  s <- replicateM l char
  return $! Text.pack s
  where
    length = MWC.uniformR (7, 20)
    char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')


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
  allTransactions <- MWC.runWithCreate $ replicateM actionsNum transactionGenerator
  defaultMain $! flip map threadsNums $! \threadsNum ->
    let
      sliceSize = actionsNum `div` threadsNum
      threadTransactions = slices sliceSize allTransactions
      in 
        bgroup
          (shows threadsNum . showString "/" . shows sliceSize $ "")
          [
            bgroup "STM Containers"
              [
                bench "Focus-based" $ 
                  scSessionRunner focusSCInterpreter threadTransactions,
                bench "Specialized" $ 
                  scSessionRunner specializedSCInterpreter threadTransactions
              ],
            bench "Unordered Containers" $
              ucSessionRunner threadTransactions
          ]
  where
    actionsNum = 100000
    threadsNums = [1, 2, 4, 6, 8, 12, 16, 32, 40, 52, 64, 80, 128]
