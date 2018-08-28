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
import qualified DeferredFolds.UnfoldlM as UnfoldlM
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

instance NFData AsciiString where
  rnf a = seq a ()

instance Hashable AsciiString where
  hashWithSalt salt (AsciiString size pa@(PrimArray ba)) =
    hashByteArrayWithSalt ba 0 (sizeofPrimArray pa) (hashWithSalt salt size)

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
  fromUnfoldlM (List.length list) (UnfoldlM.foldable list)

{-|
Convert from ByteString, ignoring each 8th bit in it.
-}
fromByteString :: ByteString -> AsciiString
fromByteString byteString = 
  fromUnfoldlM (ByteString.length byteString) (UnfoldlM.byteStringBytes byteString)

{-|
Convert from ShortByteString, ignoring each 8th bit in it.
-}
fromShortByteString :: ShortByteString -> AsciiString
fromShortByteString byteString =
  fromUnfoldlM (ShortByteString.length byteString) (UnfoldlM.shortByteStringBytes byteString)

{-# INLINE fromUnfoldlM #-}
fromUnfoldlM :: Int -> UnfoldlM IO Word8 -> AsciiString
fromUnfoldlM size unfoldM =
  AsciiString size (unsafeDupablePerformIO (UnfoldlM.foldM (FoldMs.sizedSeptetPrimArrayFromSeptets size) unfoldM))

{-| Convert to a list of septets represented by 'Word8' with the 8th bit always empty. -}
toSeptetList :: AsciiString -> [Word8]
toSeptetList = unsafeDupablePerformIO . UnfoldlM.foldM (Foldl.generalize Foldl.list) . toOctetUnfoldlM

{-| Convert to bytestring. -}
toByteString :: AsciiString -> ByteString
toByteString = unsafeDupablePerformIO . runSizedFoldM FoldMs.sizedByteStringFromOctets

{-| Convert to short bytestring. -}
toShortByteString :: AsciiString -> ShortByteString
toShortByteString = primArraySBS . unsafeDupablePerformIO . runSizedFoldM PrimArray.elementsFoldM where
  primArraySBS :: PrimArray Word8 -> ShortByteString
  primArraySBS (PrimArray ba) = ShortByteString.SBS ba

toOctetUnfoldlM :: AsciiString -> UnfoldlM IO Word8
toOctetUnfoldlM (AsciiString size pa) = let
  takeLast = rem (7 * succ size) 8 /= 0
  in UnfoldlM.mapFoldMInput (FoldMs.overSeptets takeLast) (UnfoldlM.primArray pa)

runSizedFoldM :: (Int -> FoldM IO Word8 output) -> AsciiString -> IO output
runSizedFoldM fold asciiString =
  UnfoldlM.foldM (fold (length asciiString)) (toOctetUnfoldlM asciiString)
