module Fuddle.Compiler.Parse.Layout
  ( LayoutRes(..)
  , insertLayout
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fuddle.Compiler.Base.Core (TextSize, minusSizeMay, zeroSize)
import Fuddle.Compiler.Base.Diag
  ( CodeDiag(..)
  , Diag
  , RelatedDiag(..)
  , SeverityDiag(..)
  , StageDiag(..)
  , addRelatedDiag
  , mkDiag
  )
import Fuddle.Compiler.Base.Range (Range(..))
import Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  , isTriviaKd
  )
import Fuddle.Compiler.Syntax.Token
  ( LexemeRef(..)
  , SyntheticReason(..)
  , TokenFlags
  , TokenLex(..)
  , TokenOrigin(..)
  , TokenStream
  , fromVectorToks
  , syntheticTF
  , toVectorToks
  )

-- | Layout insertion assumes the lexer emits explicit 'NewlineTk' tokens for
-- physical newlines relevant to offside handling.
data LayoutRes = LayoutRes
  { streamLR :: !TokenStream
  , diagsLR :: !(Vector Diag)
  }
  deriving (Eq, Show)

data CtxLY = CtxLY
  { colCY :: !TextSize
  , depthCY :: !Int
  , freshCY :: !Bool
  , rootCY :: !Bool
  }
  deriving (Eq, Show)

data PendingLY = PendingLY
  { kindPY :: !SyntaxKind
  , linePY :: !Int
  , refColPY :: !TextSize
  , depthPY :: !Int
  , rangePY :: !Range
  }
  deriving (Eq, Show)

data StateLY = StateLY
  { outRevLY :: ![TokenLex]
  , diagsRevLY :: ![Diag]
  , ctxsLY :: ![CtxLY]
  , pendingMayLY :: !(Maybe PendingLY)
  , lineLY :: !Int
  , lineStartLY :: !TextSize
  , lineHeadColMayLY :: !(Maybe TextSize)
  , sigLineMayLY :: !(Maybe Int)
  , depthLY :: !Int
  }
  deriving (Eq, Show)

insertLayout :: TokenStream -> LayoutRes
insertLayout stream0 =
  let toks0 = toVectorToks stream0
      st1 = V.foldl' stepTok initLY toks0
      st2 = finalizeLY (eofOff toks0) st1
      out0 = V.fromList (reverse (outRevLY st2))
      diags0 = V.fromList (reverse (diagsRevLY st2))
  in
  LayoutRes { streamLR = fromVectorToks out0, diagsLR = diags0 }

initLY :: StateLY
initLY =
  StateLY
    { outRevLY = []
    , diagsRevLY = []
    , ctxsLY = []
    , pendingMayLY = Nothing
    , lineLY = 0
    , lineStartLY = zeroSize
    , lineHeadColMayLY = Nothing
    , sigLineMayLY = Nothing
    , depthLY = 0
    }

stepTok :: StateLY -> TokenLex -> StateLY
stepTok st0 tok0
  | kindTL tok0 == EndTk =
      let off0 = startTok tok0
          st1 = flushPending off0 st0
          st2 = closeAll off0 st1
      in emitTok tok0 st2
  | isSignificantTok tok0 =
      let st1 = ensureRoot tok0 st0
          st2 = resolvePending tok0 st1
          st3 = applyLine tok0 st2
          st4 = emitTok tok0 st3
          st5 = noteSig tok0 st4
          st6 = registerPending tok0 st5
          st7 = bumpDepth tok0 st6
      in advanceLine tok0 st7
  | otherwise =
      let st1 = emitTok tok0 st0
          st2 = bumpDepth tok0 st1
      in advanceLine tok0 st2

finalizeLY :: TextSize -> StateLY -> StateLY
finalizeLY off0 st0 =
  let st1 = flushPending off0 st0
  in closeAll off0 st1

ensureRoot :: TokenLex -> StateLY -> StateLY
ensureRoot tok0 st0
  | null (ctxsLY st0) =
      let off0 = startTok tok0
          col0 = tokCol st0 tok0
          ctx0 = CtxLY { colCY = col0, depthCY = depthLY st0, freshCY = False, rootCY = True }
      in emitSyn LayoutOpenTk off0 st0 { ctxsLY = [ctx0] }
  | otherwise = st0

resolvePending :: TokenLex -> StateLY -> StateLY
resolvePending tok0 st0 =
  case pendingMayLY st0 of
    Nothing -> st0
    Just pen0 ->
      let st1 = st0 { pendingMayLY = Nothing }
          off0 = startTok tok0
          col0 = tokCol st0 tok0
      in
      if lineLY st0 == linePY pen0 then
        st1
      else if depthLY st0 /= depthPY pen0 then
        pushDiag (diagMissingBlock pen0 off0) st1
      else if col0 > refColPY pen0 then
        openCtx off0 col0 st1
      else
        pushDiag (diagBadIndent pen0 tok0) st1

applyLine :: TokenLex -> StateLY -> StateLY
applyLine tok0 st0 =
  case sigLineMayLY st0 of
    Nothing -> st0
    Just line0
      | line0 == lineLY st0 -> st0
      | otherwise ->
          let off0 = startTok tok0
              col0 = tokCol st0 tok0
              st1 = closeBefore col0 off0 st0
          in
          case ctxsLY st1 of
            ctx0 : _
              | depthCY ctx0 == depthLY st1 && colCY ctx0 == col0 ->
                  if freshCY ctx0 then
                    markTopSeen st1
                  else
                    emitSyn LayoutSepTk off0 st1
            _ -> st1

noteSig :: TokenLex -> StateLY -> StateLY
noteSig tok0 st0 =
  let col0 = tokCol st0 tok0
      headCol0 =
        case lineHeadColMayLY st0 of
          Nothing -> Just col0
          Just col1 -> Just col1
  in
  st0 { sigLineMayLY = Just (lineLY st0), lineHeadColMayLY = headCol0 }

registerPending :: TokenLex -> StateLY -> StateLY
registerPending tok0 st0
  | not (isLayoutIntroKd (kindTL tok0)) = st0
  | otherwise =
      let col0 = fromMaybe (tokCol st0 tok0) (lineHeadColMayLY st0)
          pen0 =
            PendingLY
              { kindPY = kindTL tok0
              , linePY = lineLY st0
              , refColPY = col0
              , depthPY = depthLY st0
              , rangePY = rangeTL tok0
              }
      in
      st0 { pendingMayLY = Just pen0 }

bumpDepth :: TokenLex -> StateLY -> StateLY
bumpDepth tok0 st0 =
  case kindTL tok0 of
    LParenTk -> st0 { depthLY = depthLY st0 + 1 }
    LBracketTk -> st0 { depthLY = depthLY st0 + 1 }
    LBraceTk -> st0 { depthLY = depthLY st0 + 1 }
    RParenTk -> st0 { depthLY = decDepth (depthLY st0) }
    RBracketTk -> st0 { depthLY = decDepth (depthLY st0) }
    RBraceTk -> st0 { depthLY = decDepth (depthLY st0) }
    _ -> st0

advanceLine :: TokenLex -> StateLY -> StateLY
advanceLine tok0 st0
  | kindTL tok0 == NewlineTk =
      st0
        { lineLY = lineLY st0 + 1
        , lineStartLY = endTok tok0
        , lineHeadColMayLY = Nothing
        }
  | otherwise = st0

openCtx :: TextSize -> TextSize -> StateLY -> StateLY
openCtx off0 col0 st0 =
  let ctx0 = CtxLY { colCY = col0, depthCY = depthLY st0, freshCY = True, rootCY = False }
  in emitSyn LayoutOpenTk off0 st0 { ctxsLY = ctx0 : ctxsLY st0 }

closeBefore :: TextSize -> TextSize -> StateLY -> StateLY
closeBefore col0 off0 st0 =
  case ctxsLY st0 of
    [] -> st0
    ctx0 : rest0
      | shouldCloseCtx st0 col0 ctx0 ->
          let st1 = emitSyn LayoutCloseTk off0 st0 { ctxsLY = rest0 }
          in closeBefore col0 off0 st1
      | otherwise -> st0

closeAll :: TextSize -> StateLY -> StateLY
closeAll off0 st0 =
  case ctxsLY st0 of
    [] -> st0
    _ : rest0 ->
      let st1 = emitSyn LayoutCloseTk off0 st0 { ctxsLY = rest0 }
      in closeAll off0 st1

flushPending :: TextSize -> StateLY -> StateLY
flushPending off0 st0 =
  case pendingMayLY st0 of
    Nothing -> st0
    Just pen0 ->
      let st1 = st0 { pendingMayLY = Nothing }
      in pushDiag (diagMissingBlock pen0 off0) st1

markTopSeen :: StateLY -> StateLY
markTopSeen st0 =
  case ctxsLY st0 of
    [] -> st0
    ctx0 : rest0 ->
      let ctx1 = ctx0 { freshCY = False }
      in st0 { ctxsLY = ctx1 : rest0 }

emitTok :: TokenLex -> StateLY -> StateLY
emitTok tok0 st0 = st0 { outRevLY = tok0 : outRevLY st0 }

emitSyn :: SyntaxKind -> TextSize -> StateLY -> StateLY
emitSyn kind0 off0 st0 = emitTok (mkSynTok kind0 off0) st0

pushDiag :: Diag -> StateLY -> StateLY
pushDiag diag0 st0 = st0 { diagsRevLY = diag0 : diagsRevLY st0 }

mkSynTok :: SyntaxKind -> TextSize -> TokenLex
mkSynTok kind0 off0 =
  TokenLex
    { kindTL = kind0
    , rangeTL = pointRange off0
    , lexemeRefTL = ImplicitLR
    , originTL = SyntheticTO LayoutSR
    , flagsTL = syntheticTF
    }

pointRange :: TextSize -> Range
pointRange off0 = Range { start = off0, end = off0 }

tokCol :: StateLY -> TokenLex -> TextSize
tokCol st0 tok0 = fromMaybe zeroSize (minusSizeMay (startTok tok0) (lineStartLY st0))

startTok :: TokenLex -> TextSize
startTok tok0 = start (rangeTL tok0)

endTok :: TokenLex -> TextSize
endTok tok0 = end (rangeTL tok0)

eofOff :: Vector TokenLex -> TextSize
eofOff toks0
  | V.null toks0 = zeroSize
  | otherwise = endTok (V.last toks0)

shouldCloseCtx :: StateLY -> TextSize -> CtxLY -> Bool
shouldCloseCtx st0 col0 ctx0
  | rootCY ctx0 = False
  | depthCY ctx0 > depthLY st0 = True
  | depthCY ctx0 == depthLY st0 && colCY ctx0 > col0 = True
  | otherwise = False

decDepth :: Int -> Int
decDepth depth0
  | depth0 <= 0 = 0
  | otherwise = depth0 - 1

isSignificantTok :: TokenLex -> Bool
isSignificantTok tok0 =
  let kind0 = kindTL tok0
  in kind0 /= EndTk && not (isTriviaKd kind0)

isLayoutIntroKd :: SyntaxKind -> Bool
isLayoutIntroKd kind0 =
  case kind0 of
    LetKwTk -> True
    OfKwTk -> True
    DoKwTk -> True
    CatchKwTk -> True
    WhereKwTk -> True
    _ -> False

diagBadIndent :: PendingLY -> TokenLex -> Diag
diagBadIndent pen0 tok0 =
  let msg0 = "Expected an indented block after " <> introTxt (kindPY pen0) <> "."
      rel0 = RelatedDiag { rangeRD = rangePY pen0, msgRD = "Layout-introducing token is here." }
      dg0 = mkDiag (CodeDiag "layout.expectedIndentedBlock") LayoutDG ErrorDS (rangeTL tok0) msg0
  in addRelatedDiag rel0 dg0

diagMissingBlock :: PendingLY -> TextSize -> Diag
diagMissingBlock pen0 off0 =
  let msg0 = "Expected a block after " <> introTxt (kindPY pen0) <> "."
      rel0 = RelatedDiag { rangeRD = rangePY pen0, msgRD = "Layout-introducing token is here." }
      dg0 = mkDiag (CodeDiag "layout.missingBlock") LayoutDG ErrorDS (pointRange off0) msg0
  in addRelatedDiag rel0 dg0

introTxt :: SyntaxKind -> Text
introTxt kind0 =
  case kind0 of
    LetKwTk -> "`let`"
    OfKwTk -> "`of`"
    DoKwTk -> "`do`"
    CatchKwTk -> "`catch`"
    WhereKwTk -> "`where`"
    _ -> "this construct"