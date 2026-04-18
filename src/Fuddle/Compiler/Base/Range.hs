{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Base.Range
  ( Range(..)
  , emptyRange
  , mkRange
  , widthRange
  , containsOffset
  , containsRange
  , overlapsRange
  , mergeRange
  , touchRange
  ) where

import Data.Maybe (fromMaybe)
import Fuddle.Compiler.Base.Core (TextSize, maxSize, minSize, minusSizeMay, zeroSize)

data Range = Range {
    start :: !TextSize
  , end :: !TextSize
  }
  deriving stock (Eq, Ord, Show)

emptyRange :: Range
emptyRange = Range { start = zeroSize, end = zeroSize }

mkRange :: TextSize -> TextSize -> Range
mkRange start0 end0 = Range { start = minSize start0 end0, end = maxSize start0 end0 }

widthRange :: Range -> TextSize
widthRange range0 =
  let
    range1 = normRange range0
  in
  fromMaybe zeroSize (minusSizeMay range1.end range1.start)

containsOffset :: Range -> TextSize -> Bool
containsOffset range0 off =
  let
    range1 = normRange range0
  in
  range1.start <= off && off < range1.end

containsRange :: Range -> Range -> Bool
containsRange outer0 inner0 =
  let
    outer1 = normRange outer0
    inner1 = normRange inner0
  in
  outer1.start <= inner1.start && inner1.end <= outer1.end

overlapsRange :: Range -> Range -> Bool
overlapsRange left0 right0 =
  let
    left1 = normRange left0
    right1 = normRange right0
  in
  left1.start < right1.end && right1.start < left1.end

mergeRange :: Range -> Range -> Range
mergeRange left0 right0 =
  let
    left1 = normRange left0
    right1 = normRange right0
  in
  Range { start = minSize left1.start right1.start
        , end = maxSize left1.end right1.end
    }

touchRange :: Range -> Range -> Bool
touchRange left0 right0 =
  let
    left1 = normRange left0
    right1 = normRange right0
  in
  left1.start <= right1.end && right1.start <= left1.end

normRange :: Range -> Range
normRange range0 = mkRange range0.start range0.end
