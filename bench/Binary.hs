{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative (many)
import Data.Binary.Get (getInt32le, runGet)
import Data.ByteString.Lazy qualified as BSL
import Data.CBF (CBFImage (CBFImage, imageData), decompressBinaryBSL, readCBF, decompress)
import Data.Int
import Data.Bits
import Data.Word

unconsW8 :: BSL.ByteString -> Maybe (Word8, BSL.ByteString)
unconsW8 = BSL.uncons

unconsW16 :: BSL.ByteString -> Maybe (Word16, BSL.ByteString)
unconsW16 bs = do
  (x, bs') <- unconsW8 bs
  (y, bs'') <- unconsW8 bs'
  pure (fromIntegral x .|. (fromIntegral y `shiftL` 8), bs'')

unconsW32 :: BSL.ByteString -> Maybe (Word32, BSL.ByteString)
unconsW32 bs = do
  (x, bs') <- unconsW16 bs
  (y, bs'') <- unconsW16 bs'
  pure (fromIntegral x .|. (fromIntegral y `shiftL` 16), bs'')

unconsI32 :: BSL.ByteString -> Maybe (Int32, BSL.ByteString)
unconsI32 bs = do
  (x, bs') <- unconsW32 bs
  pure (fromIntegral x, bs')

ref :: BSL.ByteString -> [Int64]
ref bs = case unconsI32 bs of
  Nothing -> []
  Just (x, bs')
    | otherwise -> fromIntegral x : ref bs'

diff :: Int -> [Int64] -> [Int64] -> [(Int, Int64, Int64)]
diff !n (x:xs) (y:ys)
  | x /= y = (n, x, y) : diff (n + 1) xs ys
  | otherwise = diff (n + 1) xs ys
diff _ [] [] = []
diff _ [] _ = error "diff: too short"
diff _ _ [] = error "diff: too long"

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
          let referenceOutputInt32 = ref referenceOutput
          print (take 10 (diff 0 decompressedVector referenceOutputInt32))
