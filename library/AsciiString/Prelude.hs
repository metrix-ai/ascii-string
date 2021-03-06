module AsciiString.Prelude
(
  module Exports,
  Product2(..),
  Product3(..),
  Product4(..),
  showBits,
)
where


-- base
-------------------------
import Control.Applicative as Exports
import Control.Arrow as Exports
import Control.Category as Exports
import Control.Concurrent as Exports
import Control.Exception as Exports
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.IO.Class as Exports
import Control.Monad.Fix as Exports hiding (fix)
import Control.Monad.ST as Exports
import Data.Bits as Exports
import Data.Bool as Exports
import Data.Char as Exports
import Data.Coerce as Exports
import Data.Complex as Exports
import Data.Data as Exports
import Data.Dynamic as Exports
import Data.Either as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports
import Data.Function as Exports hiding (id, (.))
import Data.Functor as Exports
import Data.Functor.Identity as Exports
import Data.Int as Exports
import Data.IORef as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (sortOn, isSubsequenceOf, uncons, concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Maybe as Exports
import Data.Monoid as Exports hiding (Last(..), First(..), (<>))
import Data.Ord as Exports
import Data.Proxy as Exports
import Data.Ratio as Exports
import Data.Semigroup as Exports
import Data.STRef as Exports
import Data.String as Exports
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import Data.Version as Exports
import Data.Word as Exports
import Debug.Trace as Exports
import Foreign as Exports hiding (void)
import GHC.Conc as Exports hiding (withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Exts as Exports (lazy, inline, sortWith, groupWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Numeric as Exports
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.ParserCombinators.ReadP as Exports (ReadP, ReadS, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec as Exports (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
import Text.Printf as Exports (printf, hPrintf)
import Text.Read as Exports (Read(..), readMaybe, readEither)
import Unsafe.Coerce as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)
import Data.ByteString.Short as Exports (ShortByteString)

-- primitive
-------------------------
import Data.Primitive.PrimArray as Exports hiding (sizeOf, alignment)

-- cereal
-------------------------
import Data.Serialize as Exports

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..), FoldM(..))

-- deferred-folds
-------------------------
import DeferredFolds.Unfoldl as Exports (Unfoldl(..))
import DeferredFolds.UnfoldlM as Exports (UnfoldlM(..))
import DeferredFolds.Unfoldr as Exports (Unfoldr(..))

-- hashable
-------------------------
import Data.Hashable as Exports

-- deepseq
-------------------------
import Control.DeepSeq as Exports


data Product2 a b = Product2 !a !b

data Product3 a b c = Product3 !a !b !c

data Product4 a b c d = Product4 !a !b !c !d

showBits :: forall a. (Integral a, Show a, FiniteBits a) => a -> String
showBits = padL (finiteBitSize (undefined :: a)) '0' . ($ "") . showIntAtBase 2 intToDigit

padL :: Int -> Char -> String -> String
padL n c s
    | length s < n  = s ++ replicate (n - length s) c
    | otherwise     = s
