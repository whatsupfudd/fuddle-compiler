{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Syntax.Build
  ( BuildErr(..)
  , BuildStats(..)
  , BuildRes(..)
  , buildGreen
  ) where

import Data.Bits ((.&.), (.|.), xor)
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16, Word64, Word32)
import Fuddle.Compiler.Base.Core (Hash64(..), TextSize(..), zeroSize)
import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.Base.Range (widthRange)
import Fuddle.Compiler.Syntax.Event (ParseEvent(..))
import Fuddle.Compiler.Syntax.Green
  ( ChildIx(..)
  , GreenArena(..)
  , GreenElem(..)
  , GreenFile(..)
  , GreenNode(..)
  , GreenNodeId(..)
  , GreenTok(..)
  , GreenTokId(..)
  , NodeFlags(..)
  , layoutNF
  , recoveryNF
  )
import Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  , isNodeKd
  , isRecoveryKd
  , kindTag
  )
import Fuddle.Compiler.Syntax.Token
  ( BlobKey(..)
  , LexemeRef(..)
  , SyntheticReason(..)
  , TokIx(..)
  , TokenFlags(..)
  , TokenLex(..)
  , TokenOrigin(..)
  , TokenStream
  , TextKey(..)
  , missingTF
  , syntheticTF
  , toVectorToks
  )

data BuildErr
  = TokRefOutOfRangeBE !TokIx
  | UnexpectedFinishBE
  | UnclosedNodeBE !SyntaxKind
  | EmptyTreeBE
  | EventInvalidBE !Text
  deriving stock (Eq, Show)

data BuildStats = BuildStats
  { nodesBS :: !Int
  , toksBS :: !Int
  , depthMaxBS :: !Int
  , syntheticCountBS :: !Int
  , recoveryCountBS :: !Int
  }
  deriving stock (Eq, Show)

data BuildRes = BuildRes
  { greenBR :: !GreenFile
  , diagsBR :: !(Vector Diag)
  , statsBR :: !BuildStats
  }
  deriving stock (Eq, Show)

data FrameBuild = FrameBuild
  { kindFB :: !SyntaxKind
  , kidsRevFB :: ![GreenElem]
  }

data StateBuild = StateBuild
  { framesSB :: ![FrameBuild]
  , rootMaySB :: !(Maybe GreenNodeId)
  , nodeNextSB :: !Int
  , tokNextSB :: !Int
  , childNextSB :: !Int
  , nodesSB :: !(IntMap GreenNode)
  , toksSB :: !(IntMap GreenTok)
  , childrenRevSB :: ![GreenElem]
  , diagsRevSB :: ![Diag]
  , depthCurSB :: !Int
  , depthMaxSB :: !Int
  , syntheticCountSB :: !Int
  , recoveryCountSB :: !Int
  }

buildGreen :: Hash64 -> TokenStream -> Vector ParseEvent -> Either BuildErr BuildRes
buildGreen hashSrc stream evs = do
  let toksVec = toVectorToks stream
  st1 <- go toksVec 0 initState
  case st1.framesSB of
    frame0 : _ -> Left (UnclosedNodeBE frame0.kindFB)
    [] ->
      case st1.rootMaySB of
        Nothing -> Left EmptyTreeBE
        Just rootId ->
          let arena = GreenArena
                (vecAsc st1.nodesSB)
                (vecAsc st1.toksSB)
                (V.fromList (reverse st1.childrenRevSB))
              green = GreenFile arena rootId hashSrc
              diags = V.fromList (reverse st1.diagsRevSB)
              stats = BuildStats
                { nodesBS = st1.nodeNextSB
                , toksBS = st1.tokNextSB
                , depthMaxBS = st1.depthMaxSB
                , syntheticCountBS = st1.syntheticCountSB
                , recoveryCountBS = st1.recoveryCountSB
                }
          in Right BuildRes
            { greenBR = green
            , diagsBR = diags
            , statsBR = stats
            }
  where
    go :: Vector TokenLex -> Int -> StateBuild -> Either BuildErr StateBuild
    go toksVec ix st
      | ix >= V.length evs = Right st
      | otherwise = do
          st1 <- step toksVec st (evs V.! ix)
          go toksVec (ix + 1) st1

initState :: StateBuild
initState = StateBuild
  { framesSB = []
  , rootMaySB = Nothing
  , nodeNextSB = 0
  , tokNextSB = 0
  , childNextSB = 0
  , nodesSB = IM.empty
  , toksSB = IM.empty
  , childrenRevSB = []
  , diagsRevSB = []
  , depthCurSB = 0
  , depthMaxSB = 0
  , syntheticCountSB = 0
  , recoveryCountSB = 0
  }

step :: Vector TokenLex -> StateBuild -> ParseEvent -> Either BuildErr StateBuild
step toksVec st ev =
  case ev of
    StartPE kd -> startNode kd st
    TokPE tokIx -> emitTokRef toksVec tokIx st
    TokSyntheticPE kd rsn -> emitTokSynthetic kd rsn st
    FinishPE -> finishNode st
    ErrorPE diag -> Right st { diagsRevSB = diag : st.diagsRevSB }
    TombPE -> Right st

startNode :: SyntaxKind -> StateBuild -> Either BuildErr StateBuild
startNode kd st
  | not (isNodeKd kd) = Left (EventInvalidBE ("StartPE expects a node kind, got " <> showTxt kd))
  | st.depthCurSB == 0 && hasRoot st = Left (EventInvalidBE "multiple root nodes")
  | otherwise =
      let depth1 = st.depthCurSB + 1
      in Right st
        { framesSB = FrameBuild kd [] : st.framesSB
        , depthCurSB = depth1
        , depthMaxSB = max depth1 st.depthMaxSB
        }

emitTokRef :: Vector TokenLex -> TokIx -> StateBuild -> Either BuildErr StateBuild
emitTokRef toksVec tokIx st = do
  frame0 <- needFrame st
  tokLex <- fetchTok toksVec tokIx
  let TokenLex kd rng lexRef org flg = tokLex
  if isNodeKd kd
    then Left (EventInvalidBE ("TokPE expects a token kind, got " <> showTxt kd))
    else do
      let width = widthRange rng
          tok = GreenTok kd width lexRef org flg
      st1 <- pushTok tok st
      let tokId = GreenTokId (fromIntegral (st1.tokNextSB - 1))
          frame1 = frame0 { kidsRevFB = TokGE tokId : frame0.kidsRevFB }
      Right st1 { framesSB = frame1 : tailOrBug st1.framesSB }

emitTokSynthetic :: SyntaxKind -> SyntheticReason -> StateBuild -> Either BuildErr StateBuild
emitTokSynthetic kd rsn st = do
  frame0 <- needFrame st
  if isNodeKd kd
    then Left (EventInvalidBE ("TokSyntheticPE expects a token kind, got " <> showTxt kd))
    else do
      let flg = syntheticFlags rsn
          tok = GreenTok kd zeroSize ImplicitLR (SyntheticTO rsn) flg
      st1 <- pushTok tok st
      let tokId = GreenTokId (fromIntegral (st1.tokNextSB - 1))
          frame1 = frame0 { kidsRevFB = TokGE tokId : frame0.kidsRevFB }
      Right st1 { framesSB = frame1 : tailOrBug st1.framesSB }

finishNode :: StateBuild -> Either BuildErr StateBuild
finishNode st =
  case st.framesSB of
    [] -> Left UnexpectedFinishBE
    frame0 : rest -> do
      let st0 = st { framesSB = rest, depthCurSB = st.depthCurSB - 1 }
      closeFrame frame0 st0

closeFrame :: FrameBuild -> StateBuild -> Either BuildErr StateBuild
closeFrame frame0 st =
  let kids = reverse frame0.kidsRevFB
      childCountI = length kids
      firstChild = ChildIx (fromIntegral st.childNextSB)
      width = foldl' (\acc elem0 -> acc + widthElem st elem0) zeroSize kids
      flags = flagsNode st frame0.kindFB kids
      hash = hashNodeTree st frame0.kindFB width flags kids
      node = GreenNode frame0.kindFB width firstChild (fromIntegral childCountI :: Word32) hash flags
      nodeIx = st.nodeNextSB
      nodeId = GreenNodeId (fromIntegral nodeIx)
      childrenRev1 = prependSegmentRev kids st.childrenRevSB
      recoveryInc = if isRecoveryKd frame0.kindFB then 1 else 0
      st1 = st
        { nodeNextSB = nodeIx + 1
        , childNextSB = st.childNextSB + childCountI
        , nodesSB = IM.insert nodeIx node st.nodesSB
        , childrenRevSB = childrenRev1
        , recoveryCountSB = st.recoveryCountSB + recoveryInc
        }
  in attachNode nodeId st1

attachNode :: GreenNodeId -> StateBuild -> Either BuildErr StateBuild
attachNode nodeId st =
  case st.framesSB of
    parent0 : rest ->
      let parent1 = parent0 { kidsRevFB = NodeGE nodeId : parent0.kidsRevFB }
      in Right st { framesSB = parent1 : rest }
    [] ->
      case st.rootMaySB of
        Nothing -> Right st { rootMaySB = Just nodeId }
        Just _ -> Left (EventInvalidBE "multiple root nodes")

pushTok :: GreenTok -> StateBuild -> Either BuildErr StateBuild
pushTok tok st =
  let tokIx = st.tokNextSB
      synthInc = if isSyntheticTok tok then 1 else 0
      recInc = if isRecoveryTok tok then 1 else 0
      st1 = st
        { tokNextSB = tokIx + 1
        , toksSB = IM.insert tokIx tok st.toksSB
        , syntheticCountSB = st.syntheticCountSB + synthInc
        , recoveryCountSB = st.recoveryCountSB + recInc
        }
  in Right st1

needFrame :: StateBuild -> Either BuildErr FrameBuild
needFrame st =
  case st.framesSB of
    [] -> Left (EventInvalidBE "token event outside of any open node")
    frame0 : _ -> Right frame0

fetchTok :: Vector TokenLex -> TokIx -> Either BuildErr TokenLex
fetchTok toksVec tokIx =
  let TokIx ix = tokIx
      jx = fromIntegral ix
  in if jx < 0 || jx >= V.length toksVec
       then Left (TokRefOutOfRangeBE tokIx)
       else Right (toksVec V.! jx)

hasRoot :: StateBuild -> Bool
hasRoot st =
  case st.rootMaySB of
    Nothing -> False
    Just _ -> True

tailOrBug :: [a] -> [a]
tailOrBug xs =
  case xs of
    [] -> bugBuild "tailOrBug"
    _ : rest -> rest

prependSegmentRev :: [a] -> [a] -> [a]
prependSegmentRev seg xs = foldl' (flip (:)) xs seg

widthElem :: StateBuild -> GreenElem -> TextSize
widthElem st elem0 =
  case elem0 of
    NodeGE nodeId ->
      let GreenNode _ width _ _ _ _ = lookupNodeBuilt st nodeId
      in width
    TokGE tokId ->
      let GreenTok _ width _ _ _ = lookupTokBuilt st tokId
      in width

flagsNode :: StateBuild -> SyntaxKind -> [GreenElem] -> NodeFlags
flagsNode st kd kids =
  let base = if isRecoveryKd kd then recoveryNF else zeroNF
  in foldl' (flagsElem st) base kids

flagsElem :: StateBuild -> NodeFlags -> GreenElem -> NodeFlags
flagsElem st acc elem0 =
  case elem0 of
    NodeGE nodeId ->
      let GreenNode _ _ _ _ _ flags = lookupNodeBuilt st nodeId
      in orNF acc flags
    TokGE tokId ->
      let tok = lookupTokBuilt st tokId
          acc1 = if isLayoutTok tok then orNF acc layoutNF else acc
          acc2 = if isRecoveryTok tok then orNF acc1 recoveryNF else acc1
      in acc2

lookupNodeBuilt :: StateBuild -> GreenNodeId -> GreenNode
lookupNodeBuilt st nodeId =
  let GreenNodeId ix = nodeId
  in case IM.lookup (fromIntegral ix) st.nodesSB of
       Just node -> node
       Nothing -> bugBuild ("missing green node " <> show ix)

lookupTokBuilt :: StateBuild -> GreenTokId -> GreenTok
lookupTokBuilt st tokId =
  let GreenTokId ix = tokId
  in case IM.lookup (fromIntegral ix) st.toksSB of
       Just tok -> tok
       Nothing -> bugBuild ("missing green token " <> show ix)

isSyntheticTok :: GreenTok -> Bool
isSyntheticTok tok =
  case tok of
    GreenTok _ _ _ org _ ->
      case org of
        OriginalTO -> False
        SyntheticTO _ -> True

isRecoveryTok :: GreenTok -> Bool
isRecoveryTok tok =
  case tok of
    GreenTok _ _ _ org flg ->
      case org of
        SyntheticTO RecoverySR -> True
        _ -> hasFlagTF missingTF flg

isLayoutTok :: GreenTok -> Bool
isLayoutTok tok =
  case tok of
    GreenTok kd _ _ org _ ->
      case org of
        SyntheticTO LayoutSR -> True
        _ ->
          case kd of
            LayoutOpenTk -> True
            LayoutSepTk -> True
            LayoutCloseTk -> True
            _ -> False

syntheticFlags :: SyntheticReason -> TokenFlags
syntheticFlags rsn =
  case rsn of
    RecoverySR -> orTF syntheticTF missingTF
    LayoutSR -> syntheticTF
    GeneratedSR -> syntheticTF

zeroNF :: NodeFlags
zeroNF = NodeFlags 0

orNF :: NodeFlags -> NodeFlags -> NodeFlags
orNF (NodeFlags lhs) (NodeFlags rhs) = NodeFlags (lhs .|. rhs)

orTF :: TokenFlags -> TokenFlags -> TokenFlags
orTF (TokenFlags lhs) (TokenFlags rhs) = TokenFlags (lhs .|. rhs)

hasFlagTF :: TokenFlags -> TokenFlags -> Bool
hasFlagTF (TokenFlags flg) (TokenFlags mask) = (flg .&. mask) == mask

vecAsc :: IntMap a -> Vector a
vecAsc mp = V.fromList (fmap snd (IM.toAscList mp))

hashNodeTree :: StateBuild -> SyntaxKind -> TextSize -> NodeFlags -> [GreenElem] -> Hash64
hashNodeTree st kd width flags kids =
  let h0 = hashInit 0x4e444e4f4445
      h1 = mixWord h0 (fromIntegral (kindTag kd))
      h2 = mixWord h1 (wordSize width)
      h3 = mixWord h2 (wordNodeFlags flags)
      h4 = mixWord h3 (fromIntegral (length kids))
      h5 = foldl' (mixChild st) h4 kids
  in Hash64 h5

mixChild :: StateBuild -> Word64 -> GreenElem -> Word64
mixChild st acc elem0 =
  case elem0 of
    NodeGE nodeId ->
      let GreenNode _ _ _ _ hash _ = lookupNodeBuilt st nodeId
          Hash64 childHash = hash
      in mixWord (mixWord acc 0x4e) childHash
    TokGE tokId ->
      let childHash = hashTokTree (lookupTokBuilt st tokId)
          Hash64 wordTok = childHash
      in mixWord (mixWord acc 0x54) wordTok

hashTokTree :: GreenTok -> Hash64
hashTokTree tok =
  case tok of
    GreenTok kd width lexRef org flg ->
      let h0 = hashInit 0x544f4b
          h1 = mixWord h0 (fromIntegral (kindTag kd))
          h2 = mixWord h1 (wordSize width)
          h3 = mixWord h2 (hashLexeme lexRef)
          h4 = mixWord h3 (hashOrigin org)
          h5 = mixWord h4 (wordTokenFlags flg)
      in Hash64 h5

hashLexeme :: LexemeRef -> Word64
hashLexeme lexRef =
  case lexRef of
    ImplicitLR -> 0x01
    InternLR (TextKey ix) -> 0x10 + fromIntegral ix
    BlobLR (BlobKey ix) -> 0x20 + fromIntegral ix

hashOrigin :: TokenOrigin -> Word64
hashOrigin org =
  case org of
    OriginalTO -> 0x01
    SyntheticTO rsn ->
      case rsn of
        LayoutSR -> 0x10
        RecoverySR -> 0x20
        GeneratedSR -> 0x30

hashInit :: Word64 -> Word64
hashInit seed = 14695981039346656037 `xor` seed

mixWord :: Word64 -> Word64 -> Word64
mixWord acc word = (acc `xor` word) * 1099511628211

wordSize :: TextSize -> Word64
wordSize (TextSize n) = fromIntegral n

wordNodeFlags :: NodeFlags -> Word64
wordNodeFlags (NodeFlags w) = fromIntegral w

wordTokenFlags :: TokenFlags -> Word64
wordTokenFlags (TokenFlags w) = fromIntegral w

showTxt :: Show a => a -> Text
showTxt = T.pack . show

bugBuild :: String -> a
bugBuild msg = error ("Fuddle.Compiler.Syntax.Build." <> msg)