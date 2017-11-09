module AsciiString
(
  AsciiString,
  length,
  fromSeptetList,
  fromByteString,
  fromShortByteString,
  toSeptetList,
  toByteString,
  toShortByteString,
)
where

import AsciiString.Prelude hiding (length)
import qualified Data.List as List
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short.Internal as ShortByteString
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified AsciiString.FoldMs as FoldMs
import qualified Control.Foldl as Foldl
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified Data.Serialize as Serialize


{-|
Compact representation of ASCII string.
-}
data AsciiString = AsciiString {-# UNPACK #-} !Int {-# UNPACK #-} !(PrimArray Word8)

deriving instance Eq AsciiString
deriving instance Ord AsciiString
deriving instance Generic AsciiString

instance Hashable AsciiString where
  hashWithSalt salt (AsciiString size (PrimArray ba)) =
    hashByteArrayWithSalt ba 0 size salt

instance Show AsciiString where
  show = show . toByteString

instance IsString AsciiString where
  fromString = fromSeptetList . fmap (fromIntegral . ord)

instance Serialize AsciiString where
  put (AsciiString size (PrimArray ba)) = do
    Serialize.put size
    Serialize.putShortByteString (ShortByteString.SBS ba)
  get = do
    septetsAmount <- Serialize.get
    let octetsAmount = div ((septetsAmount + 1) * 7) 8
    ShortByteString.SBS ba <- Serialize.getShortByteString octetsAmount
    return (AsciiString septetsAmount (PrimArray ba))


{-| Get the amount of septets. -}
length :: AsciiString -> Int
length (AsciiString length _) = length

{-| Construct from a list of septets encoded in 'Word8', ignoring the 8th bit. -}
fromSeptetList :: [Word8] -> AsciiString
fromSeptetList list = 
  fromUnfoldM (List.length list) (UnfoldM.foldable list)

{-|
Convert from ByteString, ignoring each 8th bit in it.
-}
fromByteString :: ByteString -> AsciiString
fromByteString byteString = 
  fromUnfoldM (ByteString.length byteString) (UnfoldM.byteStringBytes byteString)

{-|
Convert from ShortByteString, ignoring each 8th bit in it.
-}
fromShortByteString :: ShortByteString -> AsciiString
fromShortByteString byteString =
  fromUnfoldM (ShortByteString.length byteString) (UnfoldM.shortByteStringBytes byteString)

{-# INLINE fromUnfoldM #-}
fromUnfoldM :: Int -> UnfoldM IO Word8 -> AsciiString
fromUnfoldM size unfoldM =
  AsciiString size (unsafeDupablePerformIO (UnfoldM.foldM (FoldMs.sizedSeptetPrimArrayFromSeptets size) unfoldM))

{-| Convert to a list of septets represented by 'Word8' with the 8th bit always empty. -}
toSeptetList :: AsciiString -> [Word8]
toSeptetList = unsafeDupablePerformIO . UnfoldM.foldM (Foldl.generalize Foldl.list) . toOctetUnfoldM

{-| Convert to bytestring. -}
toByteString :: AsciiString -> ByteString
toByteString = unsafeDupablePerformIO . runSizedFoldM FoldMs.sizedByteStringFromOctets

{-| Convert to short bytestring. -}
toShortByteString :: AsciiString -> ShortByteString
toShortByteString = primArraySBS . unsafeDupablePerformIO . runSizedFoldM PrimArray.elementsFoldM where
  primArraySBS :: PrimArray Word8 -> ShortByteString
  primArraySBS (PrimArray ba) = ShortByteString.SBS ba

toOctetUnfoldM :: AsciiString -> UnfoldM IO Word8
toOctetUnfoldM (AsciiString size pa) = let
  takeLast = rem (7 * succ size) 8 /= 0
  in UnfoldM.mapFoldMInput (FoldMs.overSeptets takeLast) (UnfoldM.primArray pa)

runSizedFoldM :: (Int -> FoldM IO Word8 output) -> AsciiString -> IO output
runSizedFoldM fold asciiString =
  UnfoldM.foldM (fold (length asciiString)) (toOctetUnfoldM asciiString)
