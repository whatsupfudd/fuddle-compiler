{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Source.Internal.Rope
  ( Rope
  , CheckErr(..)
  , emptyRope
  , nullRope
  , sizeRope
  , depthRope
  , chunkCountRope
  , fromBytes
  , fromChunks
  , toBytes
  , toChunks
  , appendRope
  , concatRopes
  , splitAtRope
  , takeRope
  , dropRope
  , sliceRope
  , insertAtRope
  , deleteRangeRope
  , replaceRangeRope
  , checkRope
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Fuddle.Compiler.Base.Core (TextSize, fromIntSize, minusSizeMay, plusSize, toIntSize, zeroSize)
import Fuddle.Compiler.Base.Range (Range(..), mkRange, widthRange)

data Rope
  = EmptyRP
  | LeafRP { size :: !TextSize, bytes :: !ByteString }
  | NodeRP { size :: !TextSize, depth :: !Int, left :: !Rope, right :: !Rope }
  deriving stock (Eq, Show)

data CheckErr
  = EmptyLeafCE
  | WideLeafCE !Int
  | EmptyChildCE
  | SizeCacheCE !TextSize !TextSize
  | DepthCacheCE !Int !Int
  deriving stock (Eq, Show)

emptyRope :: Rope
emptyRope = EmptyRP

nullRope :: Rope -> Bool
nullRope rope =
  case rope of
    EmptyRP -> True
    _ -> False

sizeRope :: Rope -> TextSize
sizeRope rope =
  case rope of
    EmptyRP -> zeroSize
    LeafRP { size = size0 } -> size0
    NodeRP { size = size0 } -> size0

depthRope :: Rope -> Int
depthRope rope =
  case rope of
    EmptyRP -> 0
    LeafRP {} -> 1
    NodeRP { depth = depth0 } -> depth0

chunkCountRope :: Rope -> Int
chunkCountRope rope =
  case rope of
    EmptyRP -> 0
    LeafRP {} -> 1
    NodeRP { left = left0, right = right0 } -> chunkCountRope left0 + chunkCountRope right0

fromBytes :: ByteString -> Rope
fromBytes bytes0 = fromChunks [bytes0]

fromChunks :: [ByteString] -> Rope
fromChunks chunks0 =
  let
    chunks1 = packChunksRP (splitChunksRP chunks0)
    leaves = map mkLeafRP chunks1
  in
  buildForestRP leaves

toBytes :: Rope -> ByteString
toBytes = BS.concat . toChunks

toChunks :: Rope -> [ByteString]
toChunks rope0 = go rope0 []
  where
    go :: Rope -> [ByteString] -> [ByteString]
    go rope1 acc =
      case rope1 of
        EmptyRP -> acc
        LeafRP { bytes = bytes0 } -> bytes0 : acc
        NodeRP { left = left0, right = right0 } -> go left0 (go right0 acc)

appendRope :: Rope -> Rope -> Rope
appendRope left0 right0
  | nullRope left0 = right0
  | nullRope right0 = left0
  | otherwise =
      let
        rope0 = mkNodeRP left0 right0
      in
      if shouldRebalanceRP left0 right0 then rebalanceRP rope0 else rope0

concatRopes :: [Rope] -> Rope
concatRopes ropes0 = buildForestRP (filter (not . nullRope) ropes0)

splitAtRope :: TextSize -> Rope -> (Rope, Rope)
splitAtRope off0 rope0 = splitGoRP (clampSizeRP off0 (sizeRope rope0)) rope0
  where
    splitGoRP :: TextSize -> Rope -> (Rope, Rope)
    splitGoRP off1 rope1
      | off1 <= zeroSize = (emptyRope, rope1)
      | off1 >= sizeRope rope1 = (rope1, emptyRope)
      | otherwise =
          case rope1 of
            EmptyRP -> (emptyRope, emptyRope)
            LeafRP { bytes = bytes0 } ->
              let
                ix = toIntSize off1
                left1 = mkLeafRP (BS.take ix bytes0)
                right1 = mkLeafRP (BS.drop ix bytes0)
              in
              (left1, right1)
            NodeRP { left = left0, right = right0 } ->
              let
                sizeLeft = sizeRope left0
              in
              case compare off1 sizeLeft of
                LT ->
                  let
                    (leftA, leftB) = splitGoRP off1 left0
                  in
                  (leftA, appendRope leftB right0)
                EQ -> (left0, right0)
                GT ->
                  let
                    offRight = minusSizeBugRP "splitAtRope" off1 sizeLeft
                    (rightA, rightB) = splitGoRP offRight right0
                  in
                  (appendRope left0 rightA, rightB)

takeRope :: TextSize -> Rope -> Rope
takeRope off0 rope0 = fst (splitAtRope off0 rope0)

dropRope :: TextSize -> Rope -> Rope
dropRope off0 rope0 = snd (splitAtRope off0 rope0)

sliceRope :: Range -> Rope -> Rope
sliceRope range0 rope0 =
  let
    range1 = mkRange range0.start range0.end
    (_, tail0) = splitAtRope range1.start rope0
    (mid0, _) = splitAtRope (widthRange range1) tail0
  in
  mid0

insertAtRope :: TextSize -> Rope -> Rope -> Rope
insertAtRope off0 ins0 rope0 =
  let
    (left0, right0) = splitAtRope off0 rope0
  in
  concatRopes [left0, ins0, right0]

deleteRangeRope :: Range -> Rope -> Rope
deleteRangeRope range0 = replaceRangeRope range0 emptyRope

replaceRangeRope :: Range -> Rope -> Rope -> Rope
replaceRangeRope range0 with0 rope0 =
  let
    range1 = mkRange range0.start range0.end
    (left0, tail0) = splitAtRope range1.start rope0
    (_, right0) = splitAtRope (widthRange range1) tail0
  in
  concatRopes [left0, with0, right0]

checkRope :: Rope -> [CheckErr]
checkRope rope0 =
  let
    (_, _, errs0) = go rope0
  in
  errs0
  where
    go :: Rope -> (TextSize, Int, [CheckErr])
    go rope1 =
      case rope1 of
        EmptyRP -> (zeroSize, 0, [])
        LeafRP { size = size0, bytes = bytes0 } ->
          let
            size1 = fromIntSize (BS.length bytes0)
            errs0 =
              emptyLeafErrRP bytes0
              ++ wideLeafErrRP bytes0
              ++ sizeErrRP size0 size1
          in
          (size1, 1, errs0)
        NodeRP { size = size0, depth = depth0, left = left0, right = right0 } ->
          let
            (sizeLeft, depthLeft, errsLeft) = go left0
            (sizeRight, depthRight, errsRight) = go right0
            size1 = plusSize sizeLeft sizeRight
            depth1 = 1 + max depthLeft depthRight
            errs0 =
              emptyChildErrRP left0 right0
              ++ sizeErrRP size0 size1
              ++ depthErrRP depth0 depth1
              ++ errsLeft
              ++ errsRight
          in
          (size1, depth1, errs0)

leafMaxBytesRP :: Int
leafMaxBytesRP = 4096

mkLeafRP :: ByteString -> Rope
mkLeafRP bytes0
  | BS.null bytes0 = EmptyRP
  | otherwise = LeafRP { size = fromIntSize (BS.length bytes0), bytes = bytes0 }

mkNodeRP :: Rope -> Rope -> Rope
mkNodeRP left0 right0
  | nullRope left0 = right0
  | nullRope right0 = left0
  | otherwise =
      case mergeLeavesRP left0 right0 of
        Just rope0 -> rope0
        Nothing ->
          NodeRP
            { size = plusSize (sizeRope left0) (sizeRope right0)
            , depth = 1 + max (depthRope left0) (depthRope right0)
            , left = left0
            , right = right0
            }

mergeLeavesRP :: Rope -> Rope -> Maybe Rope
mergeLeavesRP left0 right0 =
  case (left0, right0) of
    (LeafRP { bytes = bytesLeft }, LeafRP { bytes = bytesRight }) ->
      let
        sizeJoined = BS.length bytesLeft + BS.length bytesRight
      in
      if sizeJoined <= leafMaxBytesRP
        then Just (mkLeafRP (bytesLeft <> bytesRight))
        else Nothing
    _ -> Nothing

buildForestRP :: [Rope] -> Rope
buildForestRP ropes0 =
  case filter (not . nullRope) ropes0 of
    [] -> emptyRope
    ropes1 -> go ropes1
  where
    go :: [Rope] -> Rope
    go ropes1 =
      case ropes1 of
        [] -> emptyRope
        [rope0] -> rope0
        _ -> go (pairRP ropes1)

    pairRP :: [Rope] -> [Rope]
    pairRP ropes1 =
      case ropes1 of
        left0 : right0 : rest0 -> appendRope left0 right0 : pairRP rest0
        [rope0] -> [rope0]
        [] -> []

rebalanceRP :: Rope -> Rope
rebalanceRP = buildForestRP . map mkLeafRP . toChunks

shouldRebalanceRP :: Rope -> Rope -> Bool
shouldRebalanceRP left0 right0 = abs (depthRope left0 - depthRope right0) > 3

splitChunksRP :: [ByteString] -> [ByteString]
splitChunksRP = concatMap splitChunkRP

splitChunkRP :: ByteString -> [ByteString]
splitChunkRP bytes0
  | BS.null bytes0 = []
  | BS.length bytes0 <= leafMaxBytesRP = [bytes0]
  | otherwise =
      let
        head0 = BS.take leafMaxBytesRP bytes0
        tail0 = BS.drop leafMaxBytesRP bytes0
      in
      head0 : splitChunkRP tail0

packChunksRP :: [ByteString] -> [ByteString]
packChunksRP chunks0 =
  let
    step :: (Int, [ByteString], [ByteString]) -> ByteString -> (Int, [ByteString], [ByteString])
    step (sizeAcc, revAcc, revOut) chunk0
      | BS.null chunk0 = (sizeAcc, revAcc, revOut)
      | sizeAcc == 0 = (BS.length chunk0, [chunk0], revOut)
      | sizeAcc + BS.length chunk0 <= leafMaxBytesRP = (sizeAcc + BS.length chunk0, chunk0 : revAcc, revOut)
      | otherwise = (BS.length chunk0, [chunk0], BS.concat (reverse revAcc) : revOut)

    finish :: (Int, [ByteString], [ByteString]) -> [ByteString]
    finish (0, _, revOut) = reverse revOut
    finish (_, revAcc, revOut) = reverse (BS.concat (reverse revAcc) : revOut)
  in
  finish (foldl' step (0, [], []) chunks0)

clampSizeRP :: TextSize -> TextSize -> TextSize
clampSizeRP off0 size0
  | off0 <= zeroSize = zeroSize
  | off0 >= size0 = size0
  | otherwise = off0

minusSizeBugRP :: String -> TextSize -> TextSize -> TextSize
minusSizeBugRP fun lhs rhs =
  case minusSizeMay lhs rhs of
    Just res0 -> res0
    Nothing -> error ("Fuddle.Compiler.Source.Internal.Rope." <> fun <> ": negative TextSize subtraction")

emptyLeafErrRP :: ByteString -> [CheckErr]
emptyLeafErrRP bytes0
  | BS.null bytes0 = [EmptyLeafCE]
  | otherwise = []

wideLeafErrRP :: ByteString -> [CheckErr]
wideLeafErrRP bytes0
  | BS.length bytes0 > leafMaxBytesRP = [WideLeafCE (BS.length bytes0)]
  | otherwise = []

emptyChildErrRP :: Rope -> Rope -> [CheckErr]
emptyChildErrRP left0 right0
  | nullRope left0 || nullRope right0 = [EmptyChildCE]
  | otherwise = []

sizeErrRP :: TextSize -> TextSize -> [CheckErr]
sizeErrRP cached0 actual0
  | cached0 == actual0 = []
  | otherwise = [SizeCacheCE cached0 actual0]

depthErrRP :: Int -> Int -> [CheckErr]
depthErrRP cached0 actual0
  | cached0 == actual0 = []
  | otherwise = [DepthCacheCE cached0 actual0]