module AsciiString.FoldMs
where

import AsciiString.Prelude
import qualified Data.ByteString.Internal as ByteString


{-|
Converts a fold over septets (still encoded in octets) into a fold over actual octets.
-}
overSeptets :: Monad m => Bool -> FoldM m Word8 x -> FoldM m Word8 x
overSeptets takeLast (FoldM septStep septInit septExtract) = FoldM octStep octInit octExtract where
  octInit = Product3 0 0 <$> septInit
  octStep (Product3 unconsumedBitsAmount unconsumedOctet septState) octet =
    case unconsumedBitsAmount of
      0 -> Product3 1 (unsafeShiftR octet 7) <$> septStep septState (clearBit octet 7)
      6 -> Product3 7 (unsafeShiftR octet 1) <$> septStep septState (unconsumedOctet .|. unsafeShiftR (unsafeShiftL octet 7) 1)
      7 -> Product3 1 (unsafeShiftR octet 7) <$> septStepTwice unconsumedOctet (clearBit octet 7)
      _ -> let
        bitsToConsume = 7 - unconsumedBitsAmount
        newUnconsumedBitsAmount = 8 - bitsToConsume
        newUnconsumedOctet = unsafeShiftR octet bitsToConsume
        septet = clearBit (unconsumedOctet .|. unsafeShiftL octet unconsumedBitsAmount) 7
        in Product3 newUnconsumedBitsAmount newUnconsumedOctet <$> septStep septState septet
    where
      septStepTwice septet1 septet2 = do
        state1 <- septStep septState septet1
        septStep state1 septet2
  octExtract (Product3 unconsumedBitsAmount unconsumedOctet septState) =
    if takeLast && unconsumedBitsAmount == 7
      then septExtract =<< septStep septState unconsumedOctet
      else septExtract septState

sizedByteStringFromOctets :: Int -> FoldM IO Word8 ByteString
sizedByteStringFromOctets size = FoldM step init extract where
  init = Product2 0 <$> mallocBytes size
  step (Product2 index ptr) octet = do
    poke (plusPtr ptr index) octet
    return (Product2 (succ index) ptr)
  extract (Product2 _ ptr) = do
    fp <- newForeignPtr finalizerFree ptr
    return (ByteString.PS fp 0 size)

{-# INLINABLE sizedSeptetPrimArrayFromSeptets #-}
sizedSeptetPrimArrayFromSeptets :: Int -> FoldM IO Word8 (PrimArray Word8)
sizedSeptetPrimArrayFromSeptets septetsAmount = FoldM step init extract where
  init = do
    let size = div (septetsAmount * 7 + 7) 8
    mpa <- newPrimArray size
    return (Product4 mpa 0 0 0)
  step (Product4 mpa octetIndex bitIndex unfinishedOctet) inputOctet =
    case bitIndex of
      0 -> return (Product4 mpa octetIndex 7 inputOctet)
      1 -> let
        finishedOctet = unfinishedOctet .|. unsafeShiftL inputOctet 1
        in do
          writePrimArray mpa octetIndex finishedOctet
          return (Product4 mpa (succ octetIndex) 0 0)
      _ -> let
        finishedOctet = unfinishedOctet .|. unsafeShiftL inputOctet bitIndex
        newUnfinishedOctet = unsafeShiftR inputOctet (8 - bitIndex)
        in do
          writePrimArray mpa octetIndex finishedOctet
          return (Product4 mpa (succ octetIndex) (pred bitIndex) newUnfinishedOctet)
  extract (Product4 mpa octetIndex bitIndex unfinishedOctet) = do
    when (bitIndex /= 0) (writePrimArray mpa octetIndex unfinishedOctet)
    unsafeFreezePrimArray mpa
