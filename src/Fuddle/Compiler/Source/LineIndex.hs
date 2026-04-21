{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Source.LineIndex
  ( LocSrc(..)
  , SpanSrc(..)
  , LineIndex
  , buildIndex
  , locAt
  , offsetAtMay
  , spanAt
  ) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Fuddle.Compiler.Base.Core (TextSize(..), fromIntSize, zeroSize)
import Fuddle.Compiler.Base.Range (Range(..))

-- Zero-based line and UTF-16 column, matching LSP conventions.
data LocSrc = LocSrc
  { line :: !Int32
  , colUtf16 :: !Int32
  }
  deriving stock (Eq, Ord, Show)

data SpanSrc = SpanSrc
  { start :: !LocSrc
  , end :: !LocSrc
  }
  deriving stock (Eq, Ord, Show)

data LineIndex = LineIndex
  { sizeLX :: !TextSize
  , startsLX :: !(Vector TextSize)
  , linesLX :: !(Vector LineInfo)
  }

data LineInfo = LineInfo
  { startLI :: !TextSize
  , endContentLI :: !TextSize
  , endLineLI :: !TextSize
  , colEndLI :: !Int32
  , boundsLI :: !(Vector BoundLC)
  }

data BoundLC = BoundLC
  { offLC :: !TextSize
  , colLC :: !Int32
  }

buildIndex :: ByteString -> LineIndex
buildIndex src =
  let
    !len = BS.length src
    !linesRev = iterLine len 0 0 0 [BoundLC (sizeAt 0) 0] []
    !lines0 = V.fromList (reverse linesRev)
    !starts0 = V.map (\line0 -> line0.startLI) lines0
  in
  LineIndex {
      sizeLX = sizeAt len
    , startsLX = starts0
    , linesLX = lines0
    }
  where
  iterLine :: Int -> Int -> Int -> Int32 -> [BoundLC] -> [LineInfo] -> [LineInfo]
  iterLine !len !startIx !ix !colNow !boundsRev !acc
    | ix >= len = finishLine startIx ix ix colNow boundsRev acc
    | otherwise =
        case BS.index src ix of
          0x0D ->
            let !nextIx =
                  if ix + 1 < len && BS.index src (ix + 1) == 0x0A
                    then ix + 2
                    else ix + 1
                !acc1 = finishLine startIx ix nextIx colNow boundsRev acc
            in
            iterLine len nextIx nextIx 0 [BoundLC (sizeAt nextIx) 0] acc1
          0x0A ->
            let !nextIx = ix + 1
                !acc1 = finishLine startIx ix nextIx colNow boundsRev acc
            in
            iterLine len nextIx nextIx 0 [BoundLC (sizeAt nextIx) 0] acc1
          _ ->
            let (!charBytes, !charCols) = stepUtf8 src ix
                !ix1 = ix + charBytes
                !col1 = colNow + charCols
                !bound1 = BoundLC (sizeAt ix1) col1
            in
            iterLine len startIx ix1 col1 (bound1 : boundsRev) acc


  finishLine :: Int -> Int -> Int -> Int32 -> [BoundLC] -> [LineInfo] -> [LineInfo]
  finishLine !startIx !endContentIx !endLineIx !colNow !boundsRev !acc =
    let
      !line0 = LineInfo { 
          startLI = sizeAt startIx
        , endContentLI = sizeAt endContentIx
        , endLineLI = sizeAt endLineIx
        , colEndLI = colNow
        , boundsLI = V.fromList (reverse boundsRev)
        }
    in
    line0 : acc

  sizeAt :: Int -> TextSize
  sizeAt n = TextSize (fromIntegral n)

locAt :: LineIndex -> TextSize -> LocSrc
locAt index0 off0
  | V.null index0.linesLX = LocSrc { line = 0, colUtf16 = 0 }
  | otherwise =
      let !off1 = clampOffset index0 off0
          !lineIx = findLineIx index0.startsLX off1
          !line0 = index0.linesLX V.! lineIx
          !col0 =
            if off1 >= line0.endContentLI
              then line0.colEndLI
              else
                let !boundIx = findBoundOffIx line0.boundsLI off1
                in (line0.boundsLI V.! boundIx).colLC
      in
      LocSrc
        { line = fromIntegral lineIx
        , colUtf16 = col0
        }

offsetAtMay :: LineIndex -> LocSrc -> Maybe TextSize
offsetAtMay index0 loc0 = do
  guard (loc0.line >= 0)
  guard (loc0.colUtf16 >= 0)
  let !lineIx = fromIntegral loc0.line
  guard (lineIx < V.length index0.linesLX)
  let !line0 = index0.linesLX V.! lineIx
      !bounds0 = line0.boundsLI
  boundIx <- findBoundColIxMay bounds0 loc0.colUtf16
  pure ((bounds0 V.! boundIx).offLC)

spanAt :: LineIndex -> Range -> SpanSrc
spanAt index0 range0 =
  SpanSrc
    { start = locAt index0 range0.start
    , end = locAt index0 range0.end
    }

clampOffset :: LineIndex -> TextSize -> TextSize
clampOffset index0 off0
  | off0 <= zeroSize = zeroSize
  | off0 >= index0.sizeLX = index0.sizeLX
  | otherwise = off0

findLineIx :: Vector TextSize -> TextSize -> Int
findLineIx starts0 off0 = go 0 (V.length starts0)
  where
    go :: Int -> Int -> Int
    go !lo !hi
      | lo + 1 >= hi = lo
      | otherwise =
          let !mid = lo + ((hi - lo) `div` 2)
          in
          if starts0 V.! mid <= off0
            then go mid hi
            else go lo mid

findBoundOffIx :: Vector BoundLC -> TextSize -> Int
findBoundOffIx bounds0 off0 = go 0 (V.length bounds0)
  where
    go :: Int -> Int -> Int
    go !lo !hi
      | lo + 1 >= hi = lo
      | otherwise =
          let !mid = lo + ((hi - lo) `div` 2)
          in
          if (bounds0 V.! mid).offLC <= off0
            then go mid hi
            else go lo mid

findBoundColIxMay :: Vector BoundLC -> Int32 -> Maybe Int
findBoundColIxMay bounds0 col0 = go 0 (V.length bounds0)
  where
    go :: Int -> Int -> Maybe Int
    go !lo !hi
      | lo >= hi = Nothing
      | otherwise =
          let !mid = lo + ((hi - lo) `div` 2)
              !colMid = (bounds0 V.! mid).colLC
          in
          case compare col0 colMid of
            LT -> go lo mid
            GT -> go (mid + 1) hi
            EQ -> Just mid

stepUtf8 :: ByteString -> Int -> (Int, Int32)
stepUtf8 src ix =
  let !b0 = BS.index src ix
  in
  case () of
    _
      | b0 < 0x80 -> (1, 1)
      | b0 < 0xC2 -> (1, 1)
      | b0 < 0xE0 ->
          if hasCont src (ix + 1)
            then (2, 1)
            else (1, 1)
      | b0 < 0xF0 ->
          if valid3 src ix b0
            then (3, 1)
            else (1, 1)
      | b0 < 0xF5 ->
          if valid4 src ix b0
            then (4, 2)
            else (1, 1)
      | otherwise -> (1, 1)

valid3 :: ByteString -> Int -> Word8 -> Bool
valid3 src ix b0
  | not (hasCont src (ix + 1) && hasCont src (ix + 2)) = False
  | otherwise =
      let !b1 = BS.index src (ix + 1)
      in
      case b0 of
        0xE0 -> 0xA0 <= b1 && b1 <= 0xBF
        0xED -> 0x80 <= b1 && b1 <= 0x9F
        _ -> True

valid4 :: ByteString -> Int -> Word8 -> Bool
valid4 src ix b0
  | not (hasCont src (ix + 1) && hasCont src (ix + 2) && hasCont src (ix + 3)) = False
  | otherwise =
      let !b1 = BS.index src (ix + 1)
      in
      case b0 of
        0xF0 -> 0x90 <= b1 && b1 <= 0xBF
        0xF4 -> 0x80 <= b1 && b1 <= 0x8F
        _ -> True

hasCont :: ByteString -> Int -> Bool
hasCont src ix
  | ix >= BS.length src = False
  | otherwise =
      let !b = BS.index src ix
      in 0x80 <= b && b <= 0xBF