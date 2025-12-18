module Main (main) where

import Control.Applicative (many)
import Data.Binary.Get (getInt32le, runGet)
import Data.ByteString.Lazy qualified as BSL
import Data.CBF (CBFImage (CBFImage, imageData), decompress, readCBF)
import Data.Vector.Unboxed qualified as V
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "Read example file" do
        contents <- readCBF "test/test_lysozyme_001_00200.cbf"
        case contents of
          Left _ -> assertFailure "couldn't read file"
          Right (CBFImage {imageData}) -> do
            let numberOfElements = 6224001
                decompressedVector = V.toList (decompress numberOfElements imageData)
            referenceOutput <- BSL.readFile "test/expected_output.bin"
            let referenceOutputInt32 = runGet (many getInt32le) referenceOutput
                difference = dropWhile (\(l, r) -> l == r) (zip (zip [0 ..] decompressedVector) (zip [0 ..] (fromIntegral <$> referenceOutputInt32)))
            print (take 10 difference)
            -- V.length decompressedVector @?= length referenceOutputInt32
            -- (fromIntegral <$> referenceOutputInt32) @?= V.toList decompressedVector
    ]
