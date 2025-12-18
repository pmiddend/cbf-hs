{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.CBF (CBFImage (..), readCBF, decompress) where

import Control.Monad (mzero, void, when)
import Control.Monad.ST (runST)
import Data.Attoparsec.ByteString.Lazy as A
import Data.Bifunctor (bimap, first)
import Data.Binary.Get (getInt16le, getInt32le, getInt64le, runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64, Int8)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeLatin1)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import Unsafe.Coerce (unsafeCoerce)

data CBFImage = CBFImage
  { imageProperties :: ![(Text.Text, Text.Text)],
    imageData :: !BSL.ByteString
  }

breakSubstringWithoutDelimiter :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
breakSubstringWithoutDelimiter needle haystack =
  let (prefix, suffix) = BS.breakSubstring needle haystack
   in (prefix, BS.drop (BS.length needle) suffix)

cbfParser :: A.Parser CBFImage
cbfParser = do
  let takeLine = do
        contents <- A.takeWhile (\x -> x /= 0xd && x /= 0xa)
        void (string "\r\n")
        pure contents
      cbfStartingLine = do
        line <- takeLine
        when ("--CIF-BINARY-FORMAT-SECTION--" `BS.isPrefixOf` line) mzero
  A.skipMany cbfStartingLine
  -- skip the starting line
  void takeLine
  let cbfPropertyLine = do
        line <- takeLine
        if BS.null line
          then mzero
          else do
            when (";" `BS.isSuffixOf` line) (void takeLine)
            pure (breakSubstringWithoutDelimiter ": " line)
  propertyLines <- A.many1 cbfPropertyLine
  -- the empty line
  void takeLine
  mapM_ A.word8 [0x0c, 0x1a, 0x04, 0xd5]
  CBFImage (bimap decodeLatin1 decodeLatin1 <$> propertyLines) <$> A.takeLazyByteString

readCBF :: FilePath -> IO (Either Text.Text CBFImage)
readCBF fn = do
  c <- BSL.readFile fn
  pure (first Text.pack (parseOnly cbfParser c))

-- decompressST :: Int -> BSL.ByteString -> ST s (MV.STVector s Int64)
decompressST :: (MV.PrimMonad m) => Int -> BSL.ByteString -> m (V.Vector Int64)
decompressST numberOfElements s = do
  mutableVector <- MV.new numberOfElements
  _ <- decompressSingleChunk s mutableVector 0 0 0
  V.freeze mutableVector

decompress :: Int -> BSL.ByteString -> V.Vector Int64
decompress numberOfElements s = runST (decompressST numberOfElements s)

decompressSingleChunk ::
  (MV.PrimMonad m) =>
  BSL.ByteString ->
  MV.MVector (MV.PrimState m) Int64 ->
  Int ->
  Int64 ->
  Int64 ->
  m ()
decompressSingleChunk s mutableVector outPos inPos value = do
  if inPos >= BSL.length s - 1
    then pure ()
    else do
      let readInt8 :: Int64 -> Maybe Int64
          readInt8 p =
            case {-# SCC "read8" #-} BSL.indexMaybe s p of
              Nothing -> Nothing
              Just v -> Just (fromIntegral (unsafeCoerce v :: Int8))
          readInt16 :: Int64 -> Maybe Int64
          readInt16 p
            | p < BSL.length s - 2 = Just $ {-# SCC "read16" #-} fromIntegral (runGet getInt16le (BSL.drop p s))
            | otherwise = Nothing
          readInt32 :: Int64 -> Maybe Int64
          readInt32 p
            | p < BSL.length s - 4 = Just $ {-# SCC "read32" #-} fromIntegral (runGet getInt32le (BSL.drop p s))
            | otherwise = Nothing
          readInt64 :: Int64 -> Maybe Int64
          readInt64 p
            | p < BSL.length s - 8 = Just $ {-# SCC "read64" #-} fromIntegral (runGet getInt64le (BSL.drop p s))
            | otherwise = Nothing
          recurse _bitDepth newInPos d = do
            if outPos >= MV.length mutableVector
              then pure ()
              else
                {-# SCC "writeV" #-}
                do
                  MV.write mutableVector outPos (value + d)
                  decompressSingleChunk s mutableVector (outPos + 1) newInPos (value + d)
      case readInt8 inPos of
        Nothing -> pure ()
        Just delta8 ->
          if -127 <= delta8 && delta8 <= 127
            then recurse (8 :: Int) (inPos + 1) delta8
            else case readInt16 (inPos + 1) of
              Nothing -> pure ()
              Just delta16 ->
                if -32767 <= delta16 && delta16 <= 32767
                  then recurse (16 :: Int) (inPos + 3) delta16
                  else case readInt32 (inPos + 3) of
                    Nothing -> pure ()
                    Just delta32 ->
                      if -2147483647 <= delta32 && delta32 <= 2147483647
                        then recurse (32 :: Int) (inPos + 7) delta32
                        else case readInt64 (inPos + 7) of
                          Nothing -> pure ()
                          Just delta64 -> recurse (64 :: Int) (inPos + 11) delta64
