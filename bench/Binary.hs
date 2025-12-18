module Main (main) where

import Control.Applicative (many)
import Data.Binary.Get (getInt32le, runGet)
import Data.ByteString.Lazy qualified as BSL
import Data.CBF (CBFImage (CBFImage, imageData), decompressBinaryBSL, readCBF)

main :: IO ()
main = do
  contents <- readCBF "test-data/test_lysozyme_001_00200.cbf"
  case contents of
    Left _ -> error "couldn't read file"
    Right (CBFImage {imageData}) -> do
      let numberOfElements = 6224001
      referenceOutput <- BSL.readFile "test-data/expected_output.bin"
      case decompressBinaryBSL numberOfElements imageData of
        Left e -> error $ "error decompressing: " <> e
        Right decompressedVector -> do
          let referenceOutputInt32 = runGet (many getInt32le) referenceOutput
              difference = dropWhile (\(l, r) -> l == r) (zip (zip [0 ..] decompressedVector) (zip [0 ..] (fromIntegral <$> referenceOutputInt32)))
          print (take 10 difference)
