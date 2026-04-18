{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Source.Internal.Utf8
  ( ViewUtf8
  , decodeUtf8
  , encodeUtf8
  , mkViewUtf8
  , fromTextUtf8
  , bytesView
  , textView
  , sizeView
  , sliceBytesMay
  , sliceTextMay
  ) where

import Control.Exception (displayException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Fuddle.Compiler.Base.Core (TextSize(..), fromIntSize)
import Fuddle.Compiler.Base.Range (Range(..), mkRange)

data ViewUtf8 = ViewUtf8
  { bytesVU :: !ByteString
  , textVU :: !Text
  , sizeVU :: !TextSize
  }
  deriving stock (Eq, Show)

decodeUtf8 :: ByteString -> Either Text Text
decodeUtf8 bs =
  case TE.decodeUtf8' bs of
    Left err -> Left (T.pack (displayException err))
    Right txt -> Right txt

encodeUtf8 :: Text -> ByteString
encodeUtf8 = TE.encodeUtf8

mkViewUtf8 :: ByteString -> Either Text ViewUtf8
mkViewUtf8 bs = do
  txt <- decodeUtf8 bs
  pure (mkViewRaw bs txt)

fromTextUtf8 :: Text -> ViewUtf8
fromTextUtf8 txt =
  let bs = encodeUtf8 txt
  in mkViewRaw bs txt

bytesView :: ViewUtf8 -> ByteString
bytesView view = view.bytesVU

textView :: ViewUtf8 -> Text
textView view = view.textVU

sizeView :: ViewUtf8 -> TextSize
sizeView view = view.sizeVU

sliceBytesMay :: Range -> ViewUtf8 -> Maybe ByteString
sliceBytesMay range0 view = do
  (startI, endI) <- boundsRangeMay (BS.length view.bytesVU) range0
  pure (BS.take (endI - startI) (BS.drop startI view.bytesVU))

sliceTextMay :: Range -> ViewUtf8 -> Maybe Text
sliceTextMay range0 view = do
  bs <- sliceBytesMay range0 view
  if BS.null bs
    then Just T.empty
    else if BS.length bs == BS.length view.bytesVU
      then Just view.textVU
      else either (const Nothing) Just (decodeUtf8 bs)

mkViewRaw :: ByteString -> Text -> ViewUtf8
mkViewRaw bs txt =
  ViewUtf8
    { bytesVU = bs
    , textVU = txt
    , sizeVU = fromIntSize (BS.length bs)
    }

boundsRangeMay :: Int -> Range -> Maybe (Int, Int)
boundsRangeMay limit range0 = do
  startI <- sizeToIntMay range1.start
  endI <- sizeToIntMay range1.end
  if startI <= endI && endI <= limit
    then Just (startI, endI)
    else Nothing
  where
    range1 = mkRange range0.start range0.end

sizeToIntMay :: TextSize -> Maybe Int
sizeToIntMay (TextSize n)
  | n < 0 = Nothing
  | otherwise = Just (fromIntegral n)