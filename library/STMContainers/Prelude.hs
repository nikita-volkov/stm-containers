module STMContainers.Prelude
( 
  module Exports,
  bug,
  bottom,
  bool,
  traversePair,
)
where

-- base
-------------------------
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, FilePath, id, (.))
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Applicative as Exports
import Control.Arrow as Exports hiding (left, right)
import Control.Category as Exports
import Data.Monoid as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports hiding (for)
import Data.Maybe as Exports
import Data.Either as Exports
import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Tuple as Exports
import Data.Function as Exports hiding ((.), id)
import Data.Ord as Exports (Down(..))
import Data.String as Exports
import Data.Int as Exports
import Data.Word as Exports
import Data.Ratio as Exports
import Data.Bits as Exports
import Data.Fixed as Exports
import Data.Ix as Exports
import Data.Data as Exports
import Data.Bool as Exports hiding (bool)
import Text.Read as Exports (readMaybe, readEither)
import Control.Exception as Exports hiding (tryJust, try, assert)
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import System.Exit as Exports
import System.IO.Unsafe as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import Unsafe.Coerce as Exports
import GHC.Conc as Exports
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import GHC.Exts as Exports (lazy, inline)
import Data.IORef as Exports
import Data.STRef as Exports
import Control.Monad.ST as Exports
import Debug.Trace as Exports hiding (traceM)

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- custom
-------------------------
import qualified Debug.Trace.LocationTH

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"stm-containers\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t

-- | A replacement for the missing 'Traverse' instance of pair in base < 4.7.
traversePair :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
traversePair f (x, y) = (,) x <$> f y
