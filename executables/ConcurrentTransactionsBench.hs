
import STMContainers.Prelude
import STMContainers.Transformers
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
import qualified Data.Set as Set


-- * Custom data structures
-------------------------

type UCMap k v = TVar (UC.HashMap k (TVar v))


-- * Transactions 
-------------------------

data TransactionF k v n where
  Insert :: v -> k -> n -> TransactionF k v n
  Delete :: k -> n -> TransactionF k v n
  Lookup :: k -> n -> TransactionF k v n
  deriving (Functor, Show)

type Transaction k v = Free (TransactionF k v)

-- * Interpreters
-------------------------

type Interpreter m = 
  forall k v r. (Hashable k, Eq k) => m k v -> Transaction k v r -> STM r

ucInterpreter :: Interpreter UCMap
ucInterpreter m = 
  iterM $ \case
    Insert v k c -> 
      do
        mv <- readTVar m
        vt <- newTVar v
        writeTVar m $! UC.insert k vt mv
        c
    Delete k c -> 
      readTVar m >>= writeTVar m . UC.delete k >> c
    Lookup k c -> 
      readTVar m >>= mapM readTVar . UC.lookup k >> c

specializedSCInterpreter :: Interpreter SC.Map
specializedSCInterpreter m =
  iterM $ \case
    Insert v k c -> SC.insert v k m >> c
    Delete k c   -> SC.delete k m >> c
    Lookup k c   -> SC.lookup k m >> c

focusSCInterpreter :: Interpreter SC.Map
focusSCInterpreter m =
  iterM $ \case
    Insert v k c -> SC.focus (Focus.insertM v) k m >> c
    Delete k c   -> SC.focus Focus.deleteM k m >> c
    Lookup k c   -> SC.focus Focus.lookupM k m >> c


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

-- |
-- Generate a list of transactions with a context of shared keys.
transactionsGroupGenerator :: Int -> Generator [Transaction Text.Text () ()]
transactionsGroupGenerator n =
  (flip evalStateT) Set.empty $ replicateM n transaction
  where
    transaction = do
      s <- size
      fmap sequence_ $ replicateM s action
      where
        size = 
          lift $ join $ weightedElementGenerator $
          [
            (1  , MWC.uniformR (8, 14)),
            (5  , MWC.uniformR (2, 7)),
            (10 , return 1)
          ]
        action =
          join $ lift $ weightedElementGenerator $
          [
            (1, delete),
            (5, insert),
            (20, lookup)
          ]
          where
            insert = do
              k <- key 10 1
              modify $ Set.insert k
              return $ liftF (Insert () k ())
            delete = do
              k <- key 1 10
              modify $ Set.delete k
              return $ liftF (Delete k ())
            lookup = do
              k <- key 1 5
              return $ liftF (Lookup k ())
            key unknownWeight knownWeight =
              join $ lift $ weightedElementGenerator $ 
              [
                (unknownWeight, unknown), 
                (knownWeight, known)
              ]
              where
                unknown = lift $ keyGenerator
                known = do
                  allKeys <- get
                  maybe unknown return =<< lift (setElementGenerator allKeys)
              
keyGenerator :: Generator Text.Text
keyGenerator = do
  l <- length
  s <- replicateM l char
  return $! Text.pack s
  where
    length = MWC.uniformR (7, 20)
    char = Char.chr <$> MWC.uniformR (Char.ord 'a', Char.ord 'z')

setElementGenerator :: Set.Set a -> Generator (Maybe a)
setElementGenerator set = do
  case Set.size set of
    0 -> return Nothing
    size -> Just . (flip Set.elemAt) set <$> MWC.uniformR (0, pred size)

elementGenerator :: [a] -> Generator a
elementGenerator = \case
  [] -> $bug "Empty list"
  l -> (!!) l <$> MWC.uniformR (0, (pred . length) l)

weightedElementGenerator :: [(Int, a)] -> Generator a
weightedElementGenerator = \case
  [] -> 
    $bug "Empty list"
  l -> 
    (flip pick) l <$> MWC.uniformR (1, total)
    where
      total = (sum . map fst) l
      pick n = \case
        (n', e) : t ->
          if n' >= n
            then e 
            else pick (n - n') t

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
  -- Pregenerate the transactions:
  transactionsGroups <- 
    MWC.runWithCreate $ replicateM (maximum threadsNums) $
    transactionsGroupGenerator (actionsNum `div` (maximum threadsNums))

  -- Run the benchmark:
  defaultMain $! flip map threadsNums $ \threadsNum -> 
    let
      session = 
        map concat $! 
        slices (length transactionsGroups `div` threadsNum) transactionsGroups
      in
        bench (shows threadsNum . showString "/" . shows (actionsNum `div` threadsNum) $ "") $
          scSessionRunner specializedSCInterpreter session
  where
    threadsNums = [1, 2, 4, 6, 8, 12, 16, 32, 64, 128]
    actionsNum = 200000
