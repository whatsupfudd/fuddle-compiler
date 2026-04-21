{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Source.Buffer
  ( OriginSrc(..)
  , VersionSrc(..)
  , Utf8Err(..)
  , BufferSrc
  , SnapshotSrc(..)
  , mkBufferUtf8
  , bytesBuf
  , textBuf
  , sizeBuf
  , sliceBytesBuf
  , sliceTextBuf
  , mkSnapshotSrc
  ) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import Fuddle.Compiler.Base.Core (Hash64(..), TextSize, fromIntSize, maxSize, minSize, toIntSize, zeroSize)
import Fuddle.Compiler.Base.Range (Range(..), mkRange, widthRange)

data OriginSrc
  = FileSO !FilePath
  | VirtualSO !Text
  | MemorySO !Text
  deriving stock (Eq, Ord, Show)

newtype VersionSrc = VersionSrc Word64
  deriving stock (Eq, Ord, Show)

data Utf8Err = InvalidUtf8UE !Text
  deriving stock (Eq, Show)

data BufferSrc = BufferSrc
  { bytesBS :: !ByteString
  , textBS :: !Text
  , sizeBS :: !TextSize
  }
  deriving stock (Eq, Show)

data SnapshotSrc = SnapshotSrc
  { originSS :: !OriginSrc
  , versionSS :: !VersionSrc
  , hashSS :: !Hash64
  , sizeSS :: !TextSize
  , bufSS :: !BufferSrc
  }
  deriving stock (Eq, Show)

mkBufferUtf8 :: ByteString -> Either Utf8Err BufferSrc
mkBufferUtf8 bytes0 =
  case TE.decodeUtf8' bytes0 of
    Left err0 -> Left (InvalidUtf8UE (T.pack (show err0)))
    Right text0 ->
      Right
        BufferSrc
          { bytesBS = bytes0
          , textBS = text0
          , sizeBS = fromIntSize (BS.length bytes0)
          }

bytesBuf :: BufferSrc -> ByteString
bytesBuf buf0 = buf0.bytesBS

textBuf :: BufferSrc -> Text
textBuf buf0 = buf0.textBS

sizeBuf :: BufferSrc -> TextSize
sizeBuf buf0 = buf0.sizeBS

sliceBytesBuf :: BufferSrc -> Range -> ByteString
sliceBytesBuf buf0 range0 =
  let range1 = clampRange buf0 range0
      startI = toIntSize range1.start
      widthI = toIntSize (widthRange range1)
  in BS.take widthI (BS.drop startI buf0.bytesBS)

sliceTextBuf :: BufferSrc -> Range -> Text
sliceTextBuf buf0 range0 =
  let range1 = clampRange buf0 range0
      bytes0 = sliceBytesBuf buf0 range1
  in case TE.decodeUtf8' bytes0 of
       Right text0 -> text0
       Left err0 ->
         error
           ( "Fuddle.Compiler.Source.Buffer.sliceTextBuf: slice is not aligned to UTF-8 boundaries for range "
           <> show range1
           <> ": "
           <> show err0
           )

mkSnapshotSrc :: OriginSrc -> VersionSrc -> BufferSrc -> SnapshotSrc
mkSnapshotSrc origin0 version0 buf0 =
  SnapshotSrc
    { originSS = origin0
    , versionSS = version0
    , hashSS = hashBytes (bytesBuf buf0)
    , sizeSS = sizeBuf buf0
    , bufSS = buf0
    }

clampRange :: BufferSrc -> Range -> Range
clampRange buf0 range0 =
  let range1 = mkRange range0.start range0.end
      limit0 = sizeBuf buf0
  in Range
       { start = clampSize limit0 range1.start
       , end = clampSize limit0 range1.end
       }

clampSize :: TextSize -> TextSize -> TextSize
clampSize hi0 x0 = maxSize zeroSize (minSize hi0 x0)

hashBytes :: ByteString -> Hash64
hashBytes bytes0 = Hash64 (BS.foldl' stepHash offsetBasisH bytes0)
  where
    offsetBasisH = 14695981039346656037
    primeH = 1099511628211

    stepHash :: Word64 -> Word8 -> Word64
    stepHash acc0 byte0 = (acc0 `xor` fromIntegral byte0) * primeH