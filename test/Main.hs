module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import AsciiString (AsciiString)
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.Serialize as Serialize
import qualified AsciiString


main =
  defaultMain $
  testGroup "All" $ let
    listGen = do
      length <- QuickCheck.choose (0, 100)
      replicateM length (QuickCheck.choose (0, 127))
    stringGen = fmap chr <$> listGen
    byteStringGen = ByteString.pack <$> listGen
    shortByteStringGen = ShortByteString.pack <$> listGen
    asciiStringGen = AsciiString.fromSeptetList <$> listGen
    in
      [
        testCase "Reduces the length" $ do
          assertEqual "" 6 (AsciiString.length (AsciiString.fromByteString "123456"))
          assertEqual "" 7 (AsciiString.length (AsciiString.fromByteString "1234567"))
          assertEqual "" 8 (AsciiString.length (AsciiString.fromByteString "12345678"))
          assertEqual "" 14 (AsciiString.length (AsciiString.fromByteString "12345678901234"))
        ,
        testProperty "List roundtrip" $ forAll listGen $ \ list ->
        list === AsciiString.toSeptetList (AsciiString.fromSeptetList list)
        ,
        testProperty "ByteString Roundtrip" $ forAll byteStringGen $ \ byteString ->
        byteString === AsciiString.toByteString (AsciiString.fromByteString byteString)
        ,
        testProperty "ShortByteString Roundtrip" $ forAll shortByteStringGen $ \ sbs ->
        sbs === AsciiString.toShortByteString (AsciiString.fromShortByteString sbs)
        ,
        testProperty "String Roundtrip" $ forAll stringGen $ \ string ->
        show string === show (fromString string :: AsciiString.AsciiString)
        ,
        testProperty "Cereal Roundtrip" $ forAll asciiStringGen $ \ asciiString ->
        Right asciiString === Serialize.decode (Serialize.encode asciiString)
        ,
        testProperty "Hashing" $ forAll asciiStringGen $ \ asciiString ->
        Right (hash asciiString) ===
        fmap (hash :: AsciiString -> Int) (Serialize.decode (Serialize.encode asciiString))
      ]
