{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Syntax.Internal.Index
  ( SyntaxIx(..)
  , NodeInfoIx(..)
  , TokInfoIx(..)
  , buildSyntaxIx
  , lookupNodeInfoIx
  , lookupTokInfoIx
  , rangeNodeIx
  , rangeTokIx
  , parentNodeMayIx
  , parentTokIx
  , firstTokNodeMayIx
  , lastTokNodeMayIx
  , prevTokMayIx
  , nextTokMayIx
  , tokenAtMayIx
  , coverNodeAtMayIx
  , coverNodeMayIx
  , ancestorsNodeIx
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word32)
import Fuddle.Compiler.Base.Core (TextSize, zeroSize)
import Fuddle.Compiler.Base.Range (Range (..), emptyRange, containsRange, mkRange)
import Fuddle.Compiler.Syntax.Green
  ( ChildIx(..)
  , GreenArena(..)
  , GreenElem(..)
  , GreenFile(..)
  , GreenNode(..)
  , GreenNodeId(..)
  , GreenTok(..)
  , GreenTokId(..)
  )

data NodeInfoIx = NodeInfoIx
  { parent :: !(Maybe GreenNodeId)
  , range :: !Range
  , slot :: !Word32
  , depth :: !Word32
  , firstTok :: !(Maybe GreenTokId)
  , lastTok :: !(Maybe GreenTokId)
  }
  deriving stock (Eq, Show)

data TokInfoIx = TokInfoIx
  { parent :: !GreenNodeId
  , range :: !Range
  , slot :: !Word32
  , ord :: !Word32
  }
  deriving stock (Eq, Show)

data SyntaxIx = SyntaxIx
  { root :: !GreenNodeId
  , nodeInfos :: !(Vector NodeInfoIx)
  , tokInfos :: !(Vector TokInfoIx)
  , tokOrder :: !(Vector GreenTokId)
  , tokStarts :: !(Vector TextSize)
  }
  deriving stock (Eq, Show)

data VisitRes = VisitRes
  { end :: !TextSize
  , ordNext :: !Word32
  , firstTok :: !(Maybe GreenTokId)
  , lastTok :: !(Maybe GreenTokId)
  }

buildSyntaxIx :: GreenFile -> SyntaxIx
buildSyntaxIx green =
  let arena0 = green.arenaGF
      nodeCount0 = V.length arena0.nodesGA
      tokCount0 = V.length arena0.toksGA
      rootId = green.rootGF
  in
  runST $ do
    when (nodeCount0 == 0) $
      failIx "buildSyntaxIx" "green arena has no nodes"

    nodeInfosMV <- MV.replicate nodeCount0 dummyNode
    tokInfosMV <- MV.replicate tokCount0 (dummyTok rootId)
    tokOrderMV <- MV.replicate tokCount0 (GreenTokId 0)
    tokStartsMV <- MV.replicate tokCount0 zeroSize

    res <- visitNode arena0 nodeInfosMV tokInfosMV tokOrderMV tokStartsMV Nothing 0 0 zeroSize rootId

    when (fromIntegral res.ordNext /= tokCount0) $
      failIx "buildSyntaxIx" $
        "token count mismatch, indexed " <> show res.ordNext <> " but arena contains " <> show tokCount0

    nodeInfosV <- V.freeze nodeInfosMV
    tokInfosV <- V.freeze tokInfosMV
    tokOrderV <- V.freeze tokOrderMV
    tokStartsV <- V.freeze tokStartsMV

    pure SyntaxIx
      { root = rootId
      , nodeInfos = nodeInfosV
      , tokInfos = tokInfosV
      , tokOrder = tokOrderV
      , tokStarts = tokStartsV
      }

lookupNodeInfoIx :: SyntaxIx -> GreenNodeId -> NodeInfoIx
lookupNodeInfoIx syntaxIx nodeId = syntaxIx.nodeInfos V.! nodeIdInt nodeId

lookupTokInfoIx :: SyntaxIx -> GreenTokId -> TokInfoIx
lookupTokInfoIx syntaxIx tokId = syntaxIx.tokInfos V.! tokIdInt tokId

rangeNodeIx :: SyntaxIx -> GreenNodeId -> Range
rangeNodeIx syntaxIx nodeId = (lookupNodeInfoIx syntaxIx nodeId).range

rangeTokIx :: SyntaxIx -> GreenTokId -> Range
rangeTokIx syntaxIx tokId = (lookupTokInfoIx syntaxIx tokId).range

parentNodeMayIx :: SyntaxIx -> GreenNodeId -> Maybe GreenNodeId
parentNodeMayIx syntaxIx nodeId = (lookupNodeInfoIx syntaxIx nodeId).parent

parentTokIx :: SyntaxIx -> GreenTokId -> GreenNodeId
parentTokIx syntaxIx tokId = (lookupTokInfoIx syntaxIx tokId).parent

firstTokNodeMayIx :: SyntaxIx -> GreenNodeId -> Maybe GreenTokId
firstTokNodeMayIx syntaxIx nodeId = (lookupNodeInfoIx syntaxIx nodeId).firstTok

lastTokNodeMayIx :: SyntaxIx -> GreenNodeId -> Maybe GreenTokId
lastTokNodeMayIx syntaxIx nodeId = (lookupNodeInfoIx syntaxIx nodeId).lastTok

prevTokMayIx :: SyntaxIx -> GreenTokId -> Maybe GreenTokId
prevTokMayIx syntaxIx tokId =
  let ord0 = fromIntegral (lookupTokInfoIx syntaxIx tokId).ord
  in
  if ord0 <= 0
    then Nothing
    else Just (syntaxIx.tokOrder V.! (ord0 - 1))

nextTokMayIx :: SyntaxIx -> GreenTokId -> Maybe GreenTokId
nextTokMayIx syntaxIx tokId =
  let ord0 = fromIntegral (lookupTokInfoIx syntaxIx tokId).ord
      ord1 = ord0 + 1
  in
  if ord1 >= V.length syntaxIx.tokOrder
    then Nothing
    else Just (syntaxIx.tokOrder V.! ord1)

tokenAtMayIx :: SyntaxIx -> TextSize -> Maybe GreenTokId
tokenAtMayIx syntaxIx off
  | V.null syntaxIx.tokStarts = Nothing
  | otherwise = seek (upperBoundSize syntaxIx.tokStarts off - 1)
  where
    seek ix0
      | ix0 < 0 = Nothing
      | otherwise =
          let tokId = syntaxIx.tokOrder V.! ix0
              range0 = (lookupTokInfoIx syntaxIx tokId).range
          in
          if range0.start <= off && off < range0.end
            then Just tokId
            else if range0.start < off
              then Nothing
              else seek (ix0 - 1)

coverNodeAtMayIx :: GreenFile -> SyntaxIx -> TextSize -> Maybe GreenNodeId
coverNodeAtMayIx green syntaxIx off = coverNodeMayIx green syntaxIx (mkRange off off)

coverNodeMayIx :: GreenFile -> SyntaxIx -> Range -> Maybe GreenNodeId
coverNodeMayIx green syntaxIx target =
  let rootId = green.rootGF
      rootRange0 = rangeNodeIx syntaxIx rootId
  in
  if not (containsRange rootRange0 target)
    then Nothing
    else Just (go rootId)
  where
    arena0 = green.arenaGF

    go nodeId =
      case lastCoveringChildMay arena0 syntaxIx nodeId target of
        Nothing -> nodeId
        Just childId -> go childId

ancestorsNodeIx :: SyntaxIx -> GreenNodeId -> [GreenNodeId]
ancestorsNodeIx syntaxIx nodeId = go (Just nodeId)
  where
    go nodeMay =
      case nodeMay of
        Nothing -> []
        Just nodeId0 ->
          let parent0 = (lookupNodeInfoIx syntaxIx nodeId0).parent
          in nodeId0 : go parent0

visitNode
  :: GreenArena
  -> MV.MVector s NodeInfoIx
  -> MV.MVector s TokInfoIx
  -> MV.MVector s GreenTokId
  -> MV.MVector s TextSize
  -> Maybe GreenNodeId
  -> Word32
  -> Word32
  -> TextSize
  -> GreenNodeId
  -> ST s VisitRes
visitNode arena0 nodeInfosMV tokInfosMV tokOrderMV tokStartsMV parentMay slot0 depth0 start0 nodeId = do
  let node0 = lookupNode arena0 nodeId
      childs0 = childrenNode arena0 nodeId

  childRes <- loopChildren childs0 0 start0 0 Nothing Nothing
  let end0 = start0 + node0.widthGN
      info0 =
        NodeInfoIx
          { parent = parentMay
          , range = mkRange start0 end0
          , slot = slot0
          , depth = depth0
          , firstTok = childRes.firstTok
          , lastTok = childRes.lastTok
          }
  MV.write nodeInfosMV (nodeIdInt nodeId) info0
  pure VisitRes
    { end = end0
    , ordNext = childRes.ordNext
    , firstTok = childRes.firstTok
    , lastTok = childRes.lastTok
    }
  where
    loopChildren childs0 childIx off0 ord0 firstTok0 lastTok0
      | childIx >= V.length childs0 =
          pure VisitRes
            { end = off0
            , ordNext = ord0
            , firstTok = firstTok0
            , lastTok = lastTok0
            }
      | otherwise =
          case childs0 V.! childIx of
            NodeGE childId -> do
              childRes <- visitNode
                arena0
                nodeInfosMV
                tokInfosMV
                tokOrderMV
                tokStartsMV
                (Just nodeId)
                (fromIntegral childIx)
                (depth0 + 1)
                off0
                childId
              let firstTok1 = firstTok0 `orTokMay` childRes.firstTok
                  lastTok1 = childRes.lastTok `orTokMayR` lastTok0
              loopChildren childs0 (childIx + 1) childRes.end childRes.ordNext firstTok1 lastTok1

            TokGE tokId -> do
              let tok0 = lookupTok arena0 tokId
                  endTok0 = off0 + tok0.widthGT
                  rangeTok0 = mkRange off0 endTok0
                  infoTok0 =
                    TokInfoIx
                      { parent = nodeId
                      , range = rangeTok0
                      , slot = fromIntegral childIx
                      , ord = ord0
                      }
              MV.write tokInfosMV (tokIdInt tokId) infoTok0
              MV.write tokOrderMV (fromIntegral ord0) tokId
              MV.write tokStartsMV (fromIntegral ord0) off0
              let firstTok1 = firstTok0 `orTokMay` Just tokId
                  lastTok1 = Just tokId
              loopChildren childs0 (childIx + 1) endTok0 (ord0 + 1) firstTok1 lastTok1

lastCoveringChildMay :: GreenArena -> SyntaxIx -> GreenNodeId -> Range -> Maybe GreenNodeId
lastCoveringChildMay arena0 syntaxIx nodeId target =
  let childs0 = childrenNode arena0 nodeId
  in go childs0 0 Nothing
  where
    go childs0 childIx bestMay
      | childIx >= V.length childs0 = bestMay
      | otherwise =
          case childs0 V.! childIx of
            TokGE _ -> go childs0 (childIx + 1) bestMay
            NodeGE childId ->
              let range0 = rangeNodeIx syntaxIx childId
                  bestMay1 =
                    if containsRange range0 target
                      then Just childId
                      else bestMay
              in go childs0 (childIx + 1) bestMay1

upperBoundSize :: Vector TextSize -> TextSize -> Int
upperBoundSize xs x = go 0 (V.length xs)
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = lo + ((hi - lo) `quot` 2)
              xMid = xs V.! mid
          in
          if xMid <= x
            then go (mid + 1) hi
            else go lo mid

lookupNode :: GreenArena -> GreenNodeId -> GreenNode
lookupNode arena0 (GreenNodeId ix0) = arena0.nodesGA V.! fromIntegral ix0

lookupTok :: GreenArena -> GreenTokId -> GreenTok
lookupTok arena0 (GreenTokId ix0) = arena0.toksGA V.! fromIntegral ix0

childrenNode :: GreenArena -> GreenNodeId -> Vector GreenElem
childrenNode arena0 nodeId =
  let node0 = lookupNode arena0 nodeId
      ChildIx firstChild0 = node0.firstChildGN
      childCount0 = fromIntegral node0.childCountGN
  in V.slice (fromIntegral firstChild0) childCount0 arena0.childrenGA

nodeIdInt :: GreenNodeId -> Int
nodeIdInt (GreenNodeId ix0) = fromIntegral ix0

tokIdInt :: GreenTokId -> Int
tokIdInt (GreenTokId ix0) = fromIntegral ix0

orTokMay :: Maybe GreenTokId -> Maybe GreenTokId -> Maybe GreenTokId
orTokMay leftMay rightMay =
  case leftMay of
    Just _ -> leftMay
    Nothing -> rightMay

orTokMayR :: Maybe GreenTokId -> Maybe GreenTokId -> Maybe GreenTokId
orTokMayR leftMay rightMay =
  case leftMay of
    Just _ -> leftMay
    Nothing -> rightMay

dummyNode :: NodeInfoIx
dummyNode =
  NodeInfoIx
    { parent = Nothing
    , range = emptyRange
    , slot = 0
    , depth = 0
    , firstTok = Nothing
    , lastTok = Nothing
    }

dummyTok :: GreenNodeId -> TokInfoIx
dummyTok rootId =
  TokInfoIx
    { parent = rootId
    , range = emptyRange
    , slot = 0
    , ord = 0
    }

failIx :: String -> String -> a
failIx fun msg = error ("Fuddle.Compiler.Syntax.Internal.Index." <> fun <> ": " <> msg)