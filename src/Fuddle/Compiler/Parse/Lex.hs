{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Parse.Lex
  ( ModeLex(..)
  , LexRes(..)
  , lexSnapshot
  ) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)

import Fuddle.Compiler.Base.Core (fromIntSize)
import Fuddle.Compiler.Base.Diag
  ( CodeDiag(..)
  , Diag
  , SeverityDiag(..)
  , StageDiag(..)
  , mkDiag
  )
import Fuddle.Compiler.Base.Range (Range(..))
import Fuddle.Compiler.Source.Buffer (SnapshotSrc, bufSS, bytesBuf)
import Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  )
import Fuddle.Compiler.Syntax.Token
  ( BlobKey(..)
  , LexemeRef(..)
  , SyntheticReason(..)
  , TextKey(..)
  , TokenFlags(..)
  , TokenLex(..)
  , TokenOrigin(..)
  , TokenStream
  , docTF
  , fromVectorToks
  , triviaTF
  )

data ModeLex =
    FullLM
  | FastLM
  deriving stock (Eq, Ord, Show)

data LexRes = LexRes
  { streamLR :: !TokenStream
  , diagsLR :: !(Vector Diag)
  }
  deriving stock (Eq, Show)

data StateLex = StateLex
  { srcSL :: !ByteString
  , sizeSL :: !Int
  , offSL :: !Int
  , toksRevSL :: ![TokenLex]
  , diagsRevSL :: ![Diag]
  , textsSL :: !(Map Text TextKey)
  , blobsSL :: !(Map Text BlobKey)
  , nextTextSL :: !Int32
  , nextBlobSL :: !Int32
  }

lexSnapshot :: ModeLex -> SnapshotSrc -> LexRes
lexSnapshot _mode snap =
  let
    src = bytesBuf (bufSS snap)
    st0 = initState src
    st1 = loopLex st0
    st2 = pushTok (fixedTok EndTk (sizeSL st1) (sizeSL st1)) st1
  in
  LexRes
    { streamLR = fromVectorToks (V.fromList (reverse (toksRevSL st2)))
    , diagsLR = V.fromList (reverse (diagsRevSL st2))
    }

initState :: ByteString -> StateLex
initState src =
  StateLex
    { srcSL = src
    , sizeSL = BS.length src
    , offSL = 0
    , toksRevSL = []
    , diagsRevSL = []
    , textsSL = M.empty
    , blobsSL = M.empty
    , nextTextSL = 0
    , nextBlobSL = 0
    }

loopLex :: StateLex -> StateLex
loopLex st
  | offSL st >= sizeSL st = st
  | otherwise = loopLex (stepLex st)

stepLex :: StateLex -> StateLex
stepLex st =
  let
    off0 = offSL st
    src = srcSL st
    b0 = BS.index src off0
  in
  if isSpaceNoNlW b0 then lexWhitespace st
  else if isNewlineW b0 then lexNewline st
  else if has2 src off0 wDash wDash
    then if has3 src off0 wDash wDash wPipe then lexLineComment True st else lexLineComment False st
  else if has3 src off0 wLBrace wDash wPipe then lexBlockComment True st
  else if has2 src off0 wLBrace wDash then lexBlockComment False st
  else if has3 src off0 wQuote wQuote wQuote then lexMultiString st
  else if b0 == wQuote then lexString st
  else if b0 == wApos then lexChar st
  else if isDigitW b0 then lexNumber st
  else if isLowerStartW b0 || isUpperStartW b0 || b0 == wUnderscore then lexName st
  else lexPunctOpOrUnknown st

lexWhitespace :: StateLex -> StateLex
lexWhitespace st =
  let
    off0 = offSL st
    end0 = scanWhileAscii (srcSL st) (sizeSL st) off0 isSpaceNoNlW
    (tok, st1) = sliceTok WhitespaceTk triviaTF off0 end0 st
  in
  advanceOff end0 (pushTok tok st1)

lexNewline :: StateLex -> StateLex
lexNewline st =
  let
    off0 = offSL st
    end0 =
      if has2 (srcSL st) off0 wCr wLf
        then off0 + 2
        else off0 + 1
    (tok, st1) = sliceTok NewlineTk triviaTF off0 end0 st
  in
  advanceOff end0 (pushTok tok st1)

lexLineComment :: Bool -> StateLex -> StateLex
lexLineComment isDoc st =
  let
    off0 = offSL st
    end0 = scanLineCommentEnd (srcSL st) (sizeSL st) off0
    flags0 = if isDoc then orTF triviaTF docTF else triviaTF
    kind0 = if isDoc then DocLineTk else CommentLineTk
    (tok, st1) = sliceTok kind0 flags0 off0 end0 st
  in
  advanceOff end0 (pushTok tok st1)

lexBlockComment :: Bool -> StateLex -> StateLex
lexBlockComment isDoc st =
  let
    off0 = offSL st
    startInner =
      if isDoc
        then off0 + 3
        else off0 + 2
    (closed0, end0) = scanBlockCommentEnd (srcSL st) (sizeSL st) startInner 1
    flags0 = if isDoc then orTF triviaTF docTF else triviaTF
    kind0 = if isDoc then DocBlockTk else CommentBlockTk
    (tok, st1) = sliceTok kind0 flags0 off0 end0 st
    st2 = advanceOff end0 (pushTok tok st1)
  in
  if closed0
    then st2
    else pushDiag (diagLex "LEX002" off0 end0 "Unterminated block comment.") st2

lexString :: StateLex -> StateLex
lexString st =
  let
    off0 = offSL st
    startChunk = off0 + 1
    (closed0, stop0, closeEnd0) = scanStringClose (srcSL st) (sizeSL st) startChunk
    openTok0 = fixedTok StringOpenTk off0 startChunk
    (chunkMay0, st1) = sliceTokMay StringChunkTk zeroTF startChunk stop0 st
  in
  if closed0
    then
      let
        closeTok0 = fixedTok StringCloseTk stop0 closeEnd0
        toks0 =
          case chunkMay0 of
            Nothing -> [openTok0, closeTok0]
            Just chunkTok0 -> [openTok0, chunkTok0, closeTok0]
      in
      advanceOff closeEnd0 (pushToks toks0 st1)
    else
      let
        toks0 =
          case chunkMay0 of
            Nothing -> [openTok0]
            Just chunkTok0 -> [openTok0, chunkTok0]
        st2 = advanceOff stop0 (pushToks toks0 st1)
      in
      pushDiag (diagLex "LEX003" off0 (max (off0 + 1) stop0) "Unterminated string literal.") st2

lexMultiString :: StateLex -> StateLex
lexMultiString st =
  let
    off0 = offSL st
    startChunk = off0 + 3
    (closed0, stop0, closeEnd0) = scanMultiStringClose (srcSL st) (sizeSL st) startChunk
    openTok0 = fixedTok MultiStringOpenTk off0 startChunk
    (chunkMay0, st1) = sliceTokMay MultiStringChunkTk zeroTF startChunk stop0 st
  in
  if closed0
    then
      let
        closeTok0 = fixedTok MultiStringCloseTk stop0 closeEnd0
        toks0 =
          case chunkMay0 of
            Nothing -> [openTok0, closeTok0]
            Just chunkTok0 -> [openTok0, chunkTok0, closeTok0]
      in
      advanceOff closeEnd0 (pushToks toks0 st1)
    else
      let
        toks0 =
          case chunkMay0 of
            Nothing -> [openTok0]
            Just chunkTok0 -> [openTok0, chunkTok0]
        st2 = advanceOff stop0 (pushToks toks0 st1)
      in
      pushDiag (diagLex "LEX004" off0 (max (off0 + 3) stop0) "Unterminated multiline string literal.") st2

lexChar :: StateLex -> StateLex
lexChar st =
  let
    off0 = offSL st
    start0 = off0 + 1
    (closed0, end0) = scanCharClose (srcSL st) (sizeSL st) start0
    tokEnd0 = end0
    (tok, st1) = sliceTok CharTk zeroTF off0 tokEnd0 st
    st2 = advanceOff tokEnd0 (pushTok tok st1)
  in
  if closed0
    then st2
    else pushDiag (diagLex "LEX005" off0 (max (off0 + 1) end0) "Unterminated character literal.") st2

lexNumber :: StateLex -> StateLex
lexNumber st =
  let
    off0 = offSL st
    endInt0 = scanDigitsUnderscore (srcSL st) (sizeSL st) off0
  in
  if hasFloatTail (srcSL st) (sizeSL st) endInt0
    then
      let
        startFrac0 = endInt0 + 1
        endFrac0 = scanDigitsUnderscore (srcSL st) (sizeSL st) startFrac0
        endNum0 = scanExponentEnd (srcSL st) (sizeSL st) endFrac0
        (tok, st1) = sliceTok FloatTk zeroTF off0 endNum0 st
      in
      advanceOff endNum0 (pushTok tok st1)
    else
      let
        (tok, st1) = sliceTok IntTk zeroTF off0 endInt0 st
      in
      advanceOff endInt0 (pushTok tok st1)

lexName :: StateLex -> StateLex
lexName st =
  let
    off0 = offSL st
    src = srcSL st
    b0 = BS.index src off0
    end0 = scanWhileAscii src (sizeSL st) off0 isNameContW
  in
  if b0 == wUnderscore
    then
      if end0 == off0 + 1
        then advanceOff end0 (pushTok (fixedTok UnderscoreTk off0 end0) st)
        else
          let
            (tok, st1) = sliceTok HoleNameTk zeroTF off0 end0 st
          in
          advanceOff end0 (pushTok tok st1)
  else if isLowerStartW b0
    then
      let
        txt0 = sliceText st off0 end0
      in
      case keywordKd txt0 of
        Just kd0 -> advanceOff end0 (pushTok (fixedTok kd0 off0 end0) st)
        Nothing ->
          let
            (tok, st1) = sliceTok LowerNameTk zeroTF off0 end0 st
          in
          advanceOff end0 (pushTok tok st1)
  else
    let
      (tok, st1) = sliceTok UpperNameTk zeroTF off0 end0 st
    in
    advanceOff end0 (pushTok tok st1)

lexPunctOpOrUnknown :: StateLex -> StateLex
lexPunctOpOrUnknown st =
  case fixedMultiMay st of
    Just (kd0, end0) -> advanceOff end0 (pushTok (fixedTok kd0 (offSL st) end0) st)
    Nothing ->
      let
        off0 = offSL st
        src = srcSL st
        b0 = BS.index src off0
      in
      case b0 of
        _ | b0 == wLParen -> emitFixed1 LParenTk st
          | b0 == wRParen -> emitFixed1 RParenTk st
          | b0 == wLBracket -> emitFixed1 LBracketTk st
          | b0 == wRBracket -> emitFixed1 RBracketTk st
          | b0 == wLBrace -> emitFixed1 LBraceTk st
          | b0 == wRBrace -> emitFixed1 RBraceTk st
          | b0 == wComma -> emitFixed1 CommaTk st
          | b0 == wSemi -> emitFixed1 SemiTk st
          | b0 == wBackslash -> emitFixed1 BackslashTk st
          | b0 == wAt -> emitFixed1 AtTk st
          | b0 == wQuestion -> emitFixed1 QuestionTk st
          | b0 == wLt -> lexLtFamily st
          | b0 == wGt -> lexGtFamily st
          | b0 == wEqual -> lexEqualFamily st
          | b0 == wBang -> lexBangFamily st
          | b0 == wColon -> lexColonFamily st
          | b0 == wDot -> lexDotFamily st
          | b0 == wPipe -> lexPipeFamily st
          | b0 == wSlash -> lexOperator st
          | b0 == wDash -> lexOperator st
          | isOpLeadOnlyW b0 -> lexOperator st
          | otherwise -> lexUnknown st

emitFixed1 :: SyntaxKind -> StateLex -> StateLex
emitFixed1 kd0 st =
  let
    off0 = offSL st
    end0 = off0 + 1
  in
  advanceOff end0 (pushTok (fixedTok kd0 off0 end0) st)

lexLtFamily :: StateLex -> StateLex
lexLtFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 LtTk st

lexGtFamily :: StateLex -> StateLex
lexGtFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 GtTk st

lexEqualFamily :: StateLex -> StateLex
lexEqualFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 EqualTk st

lexBangFamily :: StateLex -> StateLex
lexBangFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 BangTk st

lexColonFamily :: StateLex -> StateLex
lexColonFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 ColonTk st

lexDotFamily :: StateLex -> StateLex
lexDotFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 DotTk st

lexPipeFamily :: StateLex -> StateLex
lexPipeFamily st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  if end0 > off0 + 1
    then emitOperator off0 end0 st
    else emitFixed1 PipeTk st

lexOperator :: StateLex -> StateLex
lexOperator st =
  let
    off0 = offSL st
    end0 = scanOperatorEnd (srcSL st) (sizeSL st) off0
  in
  emitOperator off0 end0 st

emitOperator :: Int -> Int -> StateLex -> StateLex
emitOperator off0 end0 st =
  let
    (tok, st1) = sliceTok OperatorTk zeroTF off0 end0 st
  in
  advanceOff end0 (pushTok tok st1)

lexUnknown :: StateLex -> StateLex
lexUnknown st =
  let
    off0 = offSL st
    width0 = utf8WidthAt (srcSL st) off0
    end0 = min (sizeSL st) (off0 + width0)
    (tok, st1) = sliceTok UnknownKd zeroTF off0 end0 st
    st2 = advanceOff end0 (pushTok tok st1)
  in
  pushDiag (diagLex "LEX001" off0 end0 "Unexpected character.") st2

fixedMultiMay :: StateLex -> Maybe (SyntaxKind, Int)
fixedMultiMay st =
  let
    off0 = offSL st
    src = srcSL st
  in
  if has2 src off0 wLt wSlash then Just (LtSlashTk, off0 + 2)
  else if has2 src off0 wSlash wGt then Just (SlashGtTk, off0 + 2)
  else if has2 src off0 wColon wColon then Just (ColonColonTk, off0 + 2)
  else if has2 src off0 wDash wGt then Just (ArrowThinTk, off0 + 2)
  else if has2 src off0 wEqual wGt then Just (ArrowFatTk, off0 + 2)
  else if has2 src off0 wLt wDash then Just (ArrowLeftTk, off0 + 2)
  else if has2 src off0 wDot wDot then Just (DotDotTk, off0 + 2)
  else Nothing

scanWhileAscii :: ByteString -> Int -> Int -> (Word8 -> Bool) -> Int
scanWhileAscii src size0 off0 ok0 = go off0
  where
    go off1
      | off1 >= size0 = off1
      | ok0 (BS.index src off1) = go (off1 + 1)
      | otherwise = off1

scanLineCommentEnd :: ByteString -> Int -> Int -> Int
scanLineCommentEnd src size0 off0 = go off0
  where
    go off1
      | off1 >= size0 = off1
      | isNewlineW (BS.index src off1) = off1
      | otherwise = go (off1 + 1)

scanBlockCommentEnd :: ByteString -> Int -> Int -> Int -> (Bool, Int)
scanBlockCommentEnd src size0 off0 depth0 = go off0 depth0
  where
    go off1 depth1
      | off1 >= size0 = (False, size0)
      | has3 src off1 wLBrace wDash wPipe = go (off1 + 3) (depth1 + 1)
      | has2 src off1 wLBrace wDash = go (off1 + 2) (depth1 + 1)
      | has2 src off1 wDash wRBrace =
          if depth1 == 1
            then (True, off1 + 2)
            else go (off1 + 2) (depth1 - 1)
      | otherwise = go (off1 + utf8WidthAt src off1) depth1

scanStringClose :: ByteString -> Int -> Int -> (Bool, Int, Int)
scanStringClose src size0 off0 = go off0
  where
    go off1
      | off1 >= size0 = (False, size0, size0)
      | BS.index src off1 == wQuote = (True, off1, off1 + 1)
      | isNewlineW (BS.index src off1) = (False, off1, off1)
      | BS.index src off1 == wBackslash =
          if off1 + 1 < size0
            then go (off1 + 1 + utf8WidthAt src (off1 + 1))
            else (False, size0, size0)
      | otherwise = go (off1 + utf8WidthAt src off1)

scanMultiStringClose :: ByteString -> Int -> Int -> (Bool, Int, Int)
scanMultiStringClose src size0 off0 = go off0
  where
    go off1
      | off1 >= size0 = (False, size0, size0)
      | has3 src off1 wQuote wQuote wQuote = (True, off1, off1 + 3)
      | otherwise = go (off1 + utf8WidthAt src off1)

scanCharClose :: ByteString -> Int -> Int -> (Bool, Int)
scanCharClose src size0 off0 = go off0
  where
    go off1
      | off1 >= size0 = (False, size0)
      | BS.index src off1 == wApos = (True, off1 + 1)
      | isNewlineW (BS.index src off1) = (False, off1)
      | BS.index src off1 == wBackslash =
          if off1 + 1 < size0
            then go (off1 + 1 + utf8WidthAt src (off1 + 1))
            else (False, size0)
      | otherwise = go (off1 + utf8WidthAt src off1)

scanDigitsUnderscore :: ByteString -> Int -> Int -> Int
scanDigitsUnderscore src size0 off0 = go off0
  where
    go off1
      | off1 >= size0 = off1
      | isDigitW b1 || b1 == wUnderscore = go (off1 + 1)
      | otherwise = off1
      where
        b1 = BS.index src off1

hasFloatTail :: ByteString -> Int -> Int -> Bool
hasFloatTail src size0 off0 =
  off0 + 1 < size0
    && BS.index src off0 == wDot
    && isDigitW (BS.index src (off0 + 1))

scanExponentEnd :: ByteString -> Int -> Int -> Int
scanExponentEnd src size0 off0
  | off0 >= size0 = off0
  | not (b0 == wLowerE || b0 == wUpperE) = off0
  | otherwise =
      case expDigitsStartMay src size0 (off0 + 1) of
        Nothing -> off0
        Just off1 -> scanDigitsUnderscore src size0 off1
  where
    b0 = BS.index src off0

expDigitsStartMay :: ByteString -> Int -> Int -> Maybe Int
expDigitsStartMay src size0 off0
  | off0 >= size0 = Nothing
  | isDigitW (BS.index src off0) = Just off0
  | (BS.index src off0 == wPlus || BS.index src off0 == wDash) && off0 + 1 < size0 && isDigitW (BS.index src (off0 + 1)) = Just (off0 + 1)
  | otherwise = Nothing

scanOperatorEnd :: ByteString -> Int -> Int -> Int
scanOperatorEnd src size0 off0 = go off0
  where
    go off1
      | off1 >= size0 = off1
      | isOperatorW (BS.index src off1) = go (off1 + 1)
      | otherwise = off1

sliceTok :: SyntaxKind -> TokenFlags -> Int -> Int -> StateLex -> (TokenLex, StateLex)
sliceTok kd0 flags0 start0 end0 st =
  let
    (ref0, st1) = sliceRef start0 end0 st
  in
  ( TokenLex
      { kindTL = kd0
      , rangeTL = rangeAt start0 end0
      , lexemeRefTL = ref0
      , originTL = OriginalTO
      , flagsTL = flags0
      }
  , st1
  )

sliceTokMay :: SyntaxKind -> TokenFlags -> Int -> Int -> StateLex -> (Maybe TokenLex, StateLex)
sliceTokMay kd0 flags0 start0 end0 st
  | end0 <= start0 = (Nothing, st)
  | otherwise =
      let
        (tok0, st1) = sliceTok kd0 flags0 start0 end0 st
      in
      (Just tok0, st1)

fixedTok :: SyntaxKind -> Int -> Int -> TokenLex
fixedTok kd0 start0 end0 =
  TokenLex
    { kindTL = kd0
    , rangeTL = rangeAt start0 end0
    , lexemeRefTL = ImplicitLR
    , originTL = OriginalTO
    , flagsTL = zeroTF
    }

sliceRef :: Int -> Int -> StateLex -> (LexemeRef, StateLex)
sliceRef start0 end0 st
  | end0 <= start0 = (ImplicitLR, st)
  | end0 - start0 > blobCutoff =
      let
        txt0 = sliceText st start0 end0
      in
      case M.lookup txt0 (blobsSL st) of
        Just key0 -> (BlobLR key0, st)
        Nothing ->
          let
            key0 = BlobKey (nextBlobSL st)
            st1 =
              st
                { blobsSL = M.insert txt0 key0 (blobsSL st)
                , nextBlobSL = nextBlobSL st + 1
                }
          in
          (BlobLR key0, st1)
  | otherwise =
      let
        txt0 = sliceText st start0 end0
      in
      case M.lookup txt0 (textsSL st) of
        Just key0 -> (InternLR key0, st)
        Nothing ->
          let
            key0 = TextKey (nextTextSL st)
            st1 =
              st
                { textsSL = M.insert txt0 key0 (textsSL st)
                , nextTextSL = nextTextSL st + 1
                }
          in
          (InternLR key0, st1)

sliceText :: StateLex -> Int -> Int -> Text
sliceText st start0 end0 = TE.decodeUtf8 (BS.take (end0 - start0) (BS.drop start0 (srcSL st)))

advanceOff :: Int -> StateLex -> StateLex
advanceOff off0 st = st { offSL = off0 }

pushTok :: TokenLex -> StateLex -> StateLex
pushTok tok0 st = st { toksRevSL = tok0 : toksRevSL st }

pushToks :: [TokenLex] -> StateLex -> StateLex
pushToks toks0 st = st { toksRevSL = foldl' (flip (:)) (toksRevSL st) toks0 }

pushDiag :: Diag -> StateLex -> StateLex
pushDiag diag0 st = st { diagsRevSL = diag0 : diagsRevSL st }

diagLex :: Text -> Int -> Int -> Text -> Diag
diagLex code0 start0 end0 msg0 =
  mkDiag (CodeDiag code0) LexDG ErrorDS (rangeAt start0 end0) msg0

rangeAt :: Int -> Int -> Range
rangeAt start0 end0 =
  Range
    { start = fromIntSize start0
    , end = fromIntSize end0
    }

keywordKd :: Text -> Maybe SyntaxKind
keywordKd txt0 =
  case txt0 of
    "module" -> Just ModuleKwTk
    "import" -> Just ImportKwTk
    "exposing" -> Just ExposingKwTk
    "as" -> Just AsKwTk
    "type" -> Just TypeKwTk
    "alias" -> Just AliasKwTk
    "effect" -> Just EffectKwTk
    "foreign" -> Just ForeignKwTk
    "if" -> Just IfKwTk
    "then" -> Just ThenKwTk
    "else" -> Just ElseKwTk
    "case" -> Just CaseKwTk
    "of" -> Just OfKwTk
    "let" -> Just LetKwTk
    "in" -> Just InKwTk
    "do" -> Just DoKwTk
    "catch" -> Just CatchKwTk
    "where" -> Just WhereKwTk
    "infix" -> Just InfixKwTk
    "left" -> Just LeftKwTk
    "right" -> Just RightKwTk
    "region" -> Just RegionKwTk
    "anchor" -> Just AnchorKwTk
    "cell" -> Just CellKwTk
    "native" -> Just NativeKwTk
    "thread" -> Just ThreadKwTk
    _ -> Nothing

utf8WidthAt :: ByteString -> Int -> Int
utf8WidthAt src off0 =
  let
    b0 = BS.index src off0
  in
  if b0 < 0x80 then 1
  else if b0 < 0xE0 then 2
  else if b0 < 0xF0 then 3
  else 4

has2 :: ByteString -> Int -> Word8 -> Word8 -> Bool
has2 src off0 a0 b0 =
  off0 + 1 < BS.length src
    && BS.index src off0 == a0
    && BS.index src (off0 + 1) == b0

has3 :: ByteString -> Int -> Word8 -> Word8 -> Word8 -> Bool
has3 src off0 a0 b0 c0 =
  off0 + 2 < BS.length src
    && BS.index src off0 == a0
    && BS.index src (off0 + 1) == b0
    && BS.index src (off0 + 2) == c0

zeroTF :: TokenFlags
zeroTF = TokenFlags 0

orTF :: TokenFlags -> TokenFlags -> TokenFlags
orTF (TokenFlags lhs0) (TokenFlags rhs0) = TokenFlags (lhs0 .|. rhs0)

blobCutoff :: Int
blobCutoff = 96

isSpaceNoNlW :: Word8 -> Bool
isSpaceNoNlW b0 =
  b0 == wSpace || b0 == wTab || b0 == wVTab || b0 == wFormFeed

isNewlineW :: Word8 -> Bool
isNewlineW b0 = b0 == wLf || b0 == wCr

isDigitW :: Word8 -> Bool
isDigitW b0 = b0 >= w0 && b0 <= w9

isLowerStartW :: Word8 -> Bool
isLowerStartW b0 = b0 >= wA && b0 <= wZLower

isUpperStartW :: Word8 -> Bool
isUpperStartW b0 = b0 >= wAUpper && b0 <= wZUpper

isAlphaNumW :: Word8 -> Bool
isAlphaNumW b0 = isLowerStartW b0 || isUpperStartW b0 || isDigitW b0

isNameContW :: Word8 -> Bool
isNameContW b0 = isAlphaNumW b0 || b0 == wUnderscore || b0 == wApos

isOperatorW :: Word8 -> Bool
isOperatorW b0 =
     b0 == wBang
  || b0 == wHash
  || b0 == wDollar
  || b0 == wPercent
  || b0 == wAmp
  || b0 == wStar
  || b0 == wPlus
  || b0 == wDash
  || b0 == wDot
  || b0 == wSlash
  || b0 == wColon
  || b0 == wLt
  || b0 == wEqual
  || b0 == wGt
  || b0 == wCaret
  || b0 == wPipe
  || b0 == wTilde

isOpLeadOnlyW :: Word8 -> Bool
isOpLeadOnlyW b0 =
     b0 == wHash
  || b0 == wDollar
  || b0 == wPercent
  || b0 == wAmp
  || b0 == wStar
  || b0 == wPlus
  || b0 == wCaret
  || b0 == wTilde

w0, w9, wA, wZLower, wAUpper, wZUpper :: Word8
w0 = 48
w9 = 57
wA = 97
wZLower = 122
wAUpper = 65
wZUpper = 90

wLf, wCr, wSpace, wTab, wVTab, wFormFeed :: Word8
wLf = 10
wCr = 13
wSpace = 32
wTab = 9
wVTab = 11
wFormFeed = 12

wQuote, wApos, wBackslash :: Word8
wQuote = 34
wApos = 39
wBackslash = 92

wLParen, wRParen, wLBracket, wRBracket, wLBrace, wRBrace :: Word8
wLParen = 40
wRParen = 41
wLBracket = 91
wRBracket = 93
wLBrace = 123
wRBrace = 125

wComma, wSemi, wColon, wDot, wPipe, wEqual, wBang :: Word8
wComma = 44
wSemi = 59
wColon = 58
wDot = 46
wPipe = 124
wEqual = 61
wBang = 33

wLt, wGt, wSlash, wDash, wPlus, wStar, wPercent, wAmp, wCaret, wTilde :: Word8
wLt = 60
wGt = 62
wSlash = 47
wDash = 45
wPlus = 43
wStar = 42
wPercent = 37
wAmp = 38
wCaret = 94
wTilde = 126

wHash, wDollar, wAt, wQuestion, wUnderscore :: Word8
wHash = 35
wDollar = 36
wAt = 64
wQuestion = 63
wUnderscore = 95

wLowerE, wUpperE :: Word8
wLowerE = 101
wUpperE = 69