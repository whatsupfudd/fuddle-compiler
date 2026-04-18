{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Source.Edit
  ( EditSrc(..)
  , EditErr(..)
  , applyEdit
  , applyEdits
  ) where

import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fuddle.Compiler.Base.Core (TextSize, zeroSize)
import Fuddle.Compiler.Base.Range (Range(..))
import Fuddle.Compiler.Source.Buffer
  ( SnapshotSrc(..)
  , VersionSrc(..)
  , BufferSrc(..)
  , mkBufferUtf8
  , mkSnapshotSrc
  , sliceTextBuf
  )

data EditSrc = EditSrc
  { rangeES :: !Range
  , withES :: !Text
  }
  deriving stock (Eq, Show)

data EditErr
  = InvalidRangeEE !Range
  | OverlapEE !Range !Range
  deriving stock (Eq, Show)

applyEdit :: SnapshotSrc -> EditSrc -> Either EditErr SnapshotSrc
applyEdit snapshot edit = applyEdits snapshot (V.singleton edit)

applyEdits :: SnapshotSrc -> Vector EditSrc -> Either EditErr SnapshotSrc
applyEdits snapshot edits0
  | V.null edits0 = Right snapshot
  | otherwise = do
      plans1 <- traverse (validatePlan snapshot.sizeSS) (mkPlans edits0)
      let plans2 = sortBy comparePlan plans1
      validateOverlaps plans2
      pure (buildSnapshot snapshot plans2)

data EditPlan = EditPlan
  { ixEP :: !Int
  , rangeEP :: !Range
  , withEP :: !Text
  }

mkPlans :: Vector EditSrc -> [EditPlan]
mkPlans edits0 = go 0 (V.toList edits0)
  where
    go :: Int -> [EditSrc] -> [EditPlan]
    go _ [] = []
    go ix0 (edit : rest) =
      EditPlan { ixEP = ix0, rangeEP = edit.rangeES, withEP = edit.withES } : go (ix0 + 1) rest

validatePlan :: TextSize -> EditPlan -> Either EditErr EditPlan
validatePlan size0 plan
  | validRange size0 plan.rangeEP = Right plan
  | otherwise = Left (InvalidRangeEE plan.rangeEP)

validRange :: TextSize -> Range -> Bool
validRange size0 range0 =
  let start0 = range0.start
      end0 = range0.end
  in start0 >= zeroSize && end0 >= zeroSize && start0 <= end0 && end0 <= size0

comparePlan :: EditPlan -> EditPlan -> Ordering
comparePlan left right =
  compare left.rangeEP.start right.rangeEP.start
    <> compare left.rangeEP.end right.rangeEP.end
    <> compare left.ixEP right.ixEP

validateOverlaps :: [EditPlan] -> Either EditErr ()
validateOverlaps [] = Right ()
validateOverlaps [_] = Right ()
validateOverlaps (left : right : rest)
  | overlapsPlan left right = Left (OverlapEE left.rangeEP right.rangeEP)
  | otherwise = validateOverlaps (right : rest)

overlapsPlan :: EditPlan -> EditPlan -> Bool
overlapsPlan left right = right.rangeEP.start < left.rangeEP.end

buildSnapshot :: SnapshotSrc -> [EditPlan] -> SnapshotSrc
buildSnapshot snapshot plans =
  let
    text1 = TL.toStrict (TB.toLazyText (buildText snapshot plans))
    buf1 = mkBufferFromText text1
    version1 = bumpVersion snapshot.versionSS
  in
  mkSnapshotSrc snapshot.originSS version1 buf1

buildText :: SnapshotSrc -> [EditPlan] -> TB.Builder
buildText snapshot = go zeroSize
  where
    go :: TextSize -> [EditPlan] -> TB.Builder
    go cursor [] = segmentB cursor snapshot.sizeSS
    go cursor (plan : rest) =
      segmentB cursor plan.rangeEP.start
        <> TB.fromText plan.withEP
        <> go plan.rangeEP.end rest

    segmentB :: TextSize -> TextSize -> TB.Builder
    segmentB start0 end0
      | start0 == end0 = mempty
      | otherwise = TB.fromText (sliceTextBuf snapshot.bufSS (Range { start = start0, end = end0 }))

bumpVersion :: VersionSrc -> VersionSrc
bumpVersion (VersionSrc version0)
  | version0 == maxBound = VersionSrc maxBound
  | otherwise = VersionSrc (version0 + 1)

mkBufferFromText :: Text -> BufferSrc
mkBufferFromText text0 =
  case mkBufferUtf8 (encodeUtf8 text0) of
    Right buf1 -> buf1
    Left utf8Err ->
      error
        ( "Fuddle.Compiler.Source.Edit.mkBufferFromText: impossible UTF-8 failure after encoding Text: "
            <> show utf8Err
        )

