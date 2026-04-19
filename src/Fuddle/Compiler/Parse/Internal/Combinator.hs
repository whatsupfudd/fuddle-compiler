module Fuddle.Compiler.Parse.Internal.Combinator
  ( expectTok
  , matchTok
  , acceptTok
  , acceptTokMaybe
  , acceptWhen
  , startNd
  , finishNd
  , emitTok
  , emitSyntheticTok
  , withNode
  , sepByLayout
  ) where

import Control.Monad.State.Strict (modify')

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Fuddle.Compiler.Base.Diag
  ( CodeDiag(..)
  , Diag(..)
  , SeverityDiag(..)
  , StageDiag(..)
  , mkDiag
  )
import Fuddle.Compiler.Base.Range (emptyRange)
import Fuddle.Compiler.Parse.Internal.State
  ( Parser
  , StateParse(..)
  , bumpTok
  , markErr
  , peekTokMay
  , softFail
  )
import Fuddle.Compiler.Syntax.Event (ParseEvent(..))
import Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  , isNodeKd
  , isTokenKd
  )
import Fuddle.Compiler.Syntax.Token
  ( SyntheticReason(..)
  , TokenLex(..)
  )

expectTok :: SyntaxKind -> Parser Bool
expectTok kd
  | not (isTokenKd kd) = bugCombinator ("expectTok: expected token kind, got " <> show kd)
  | otherwise = do
      ok <- matchTok kd
      if ok
        then pure True
        else do
          tokMay <- peekTokMay
          markErr (expectedTokDiag kd tokMay)
          emitSyntheticTok kd RecoverySR
          pure False

matchTok :: SyntaxKind -> Parser Bool
matchTok kd
  | not (isTokenKd kd) = bugCombinator ("matchTok: expected token kind, got " <> show kd)
  | otherwise = do
      tokMay <- peekTokMay
      case tokMay of
        Just (_, tok) | tok.kindTL == kd -> emitTok
        _ -> pure False


acceptTok :: SyntaxKind -> Parser ()
acceptTok kd
  | not (isTokenKd kd) = bugCombinator ("acceptTok: expected token kind, got " <> show kd)
  | otherwise = do
      tokMay <- peekTokMay
      case tokMay of
        Just (_, tok) | tok.kindTL == kd -> do
          _ <- emitTok
          pure ()
        _ ->
          softFail

acceptTokMaybe :: SyntaxKind -> Parser Bool
acceptTokMaybe kd
  | not (isTokenKd kd) = bugCombinator ("acceptTokMaybe: expected token kind, got " <> show kd)
  | otherwise = do
      tokMay <- peekTokMay
      case tokMay of
        Just (_, tok) | tok.kindTL == kd -> emitTok
        _ -> pure False


acceptWhen :: (TokenLex -> Bool) -> Parser ()
acceptWhen p = do
  tokMay <- peekTokMay
  case tokMay of
    Just (_, tok) | p tok -> do
      _ <- emitTok
      pure ()
    _ -> softFail


startNd :: SyntaxKind -> Parser ()
startNd kd
  | not (isNodeKd kd) = bugCombinator ("startNd: expected node kind, got " <> show kd)
  | otherwise = pushEvent (StartPE kd)

finishNd :: Parser ()
finishNd = pushEvent FinishPE

emitTok :: Parser Bool
emitTok = do
  tokMay <- bumpTok
  case tokMay of
    Just (tokIx, _) -> do
      pushEvent (TokPE tokIx)
      pure True
    Nothing ->
      pure False

emitSyntheticTok :: SyntaxKind -> SyntheticReason -> Parser ()
emitSyntheticTok kd sr
  | not (isTokenKd kd) = bugCombinator ("emitSyntheticTok: expected token kind, got " <> show kd)
  | otherwise = pushEvent (TokSyntheticPE kd sr)

withNode :: SyntaxKind -> Parser a -> Parser a
withNode kd act = do
  startNd kd
  res <- act
  finishNd
  pure res

sepByLayout :: Parser a -> Parser [a]
sepByLayout itemP = do
  _ <- expectTok LayoutOpenTk
  closed0 <- matchTok LayoutCloseTk
  if closed0 then pure [] else loop []
 where
  loop accRev = do
    item <- itemP
    tailItems (item : accRev)

  tailItems accRev = do
    sep <- matchTok LayoutSepTk
    if sep
      then do
        closed <- matchTok LayoutCloseTk
        if closed
          then pure (reverse accRev)
          else do
            item <- itemP
            tailItems (item : accRev)
      else do
        _ <- expectTok LayoutCloseTk
        pure (reverse accRev)

pushEvent :: ParseEvent -> Parser ()
pushEvent ev =
  modify' (\st -> st { eventsRevSP = ev : st.eventsRevSP })

expectedTokDiag :: SyntaxKind -> Maybe (a, TokenLex) -> Diag
expectedTokDiag wantKd gotMay =
  let range0 = maybe emptyRange (\(_, tok) -> tok.rangeTL) gotMay
      wantTxt = renderKd wantKd
      msg0 =
        case gotMay of
          Nothing ->
            "expected " <> wantTxt
          Just (_, tok) ->
            if tok.kindTL == EndTk
              then "expected " <> wantTxt <> " before end of input"
              else "expected " <> wantTxt <> ", found " <> renderKd tok.kindTL
      diag0 = mkDiag (CodeDiag "parse.expected-token") ParseDG ErrorDS range0 msg0
  in diag0 { notesDG = V.singleton "the parser inserted a synthetic token to continue parsing" }

renderKd :: SyntaxKind -> Text
renderKd kd =
  case kd of
    UnknownKd -> "<unknown>"
    EndTk -> "end of input"
    WhitespaceTk -> "whitespace"
    NewlineTk -> "newline"
    CommentLineTk -> "line comment"
    CommentBlockTk -> "block comment"
    DocLineTk -> "doc line comment"
    DocBlockTk -> "doc block comment"
    LayoutOpenTk -> "<layout-open>"
    LayoutSepTk -> "<layout-sep>"
    LayoutCloseTk -> "<layout-close>"

    LowerNameTk -> "lowercase name"
    UpperNameTk -> "uppercase name"
    HoleNameTk -> "hole name"
    OperatorTk -> "operator"
    IntTk -> "integer literal"
    FloatTk -> "float literal"
    CharTk -> "char literal"
    StringOpenTk -> "'\"'"
    StringChunkTk -> "string chunk"
    StringCloseTk -> "'\"'"
    MultiStringOpenTk -> "\"\"\""
    MultiStringChunkTk -> "multi-line string chunk"
    MultiStringCloseTk -> "\"\"\""
    InterpOpenTk -> "'${'"
    InterpCloseTk -> "'}'"

    ModuleKwTk -> "'module'"
    ImportKwTk -> "'import'"
    ExposingKwTk -> "'exposing'"
    AsKwTk -> "'as'"
    TypeKwTk -> "'type'"
    AliasKwTk -> "'alias'"
    EffectKwTk -> "'effect'"
    ForeignKwTk -> "'foreign'"
    IfKwTk -> "'if'"
    ThenKwTk -> "'then'"
    ElseKwTk -> "'else'"
    CaseKwTk -> "'case'"
    OfKwTk -> "'of'"
    LetKwTk -> "'let'"
    InKwTk -> "'in'"
    DoKwTk -> "'do'"
    CatchKwTk -> "'catch'"
    WhereKwTk -> "'where'"
    InfixKwTk -> "'infix'"
    LeftKwTk -> "'left'"
    RightKwTk -> "'right'"
    RegionKwTk -> "'region'"
    AnchorKwTk -> "'anchor'"
    CellKwTk -> "'cell'"
    NativeKwTk -> "'native'"
    ThreadKwTk -> "'thread'"

    LParenTk -> "'('"
    RParenTk -> "')'"
    LBracketTk -> "'['"
    RBracketTk -> "']'"
    LBraceTk -> "'{'"
    RBraceTk -> "'}'"
    LtTk -> "'<'"
    GtTk -> "'>'"
    LtSlashTk -> "'</'"
    SlashGtTk -> "'/>'"
    CommaTk -> "','"
    ColonTk -> "':'"
    ColonColonTk -> "'::'"
    SemiTk -> "';'"
    DotTk -> "'.'"
    DotDotTk -> "'..'"
    PipeTk -> "'|'"
    EqualTk -> "'='"
    BangTk -> "'!'"
    BackslashTk -> "'\'"
    ArrowThinTk -> "'->'"
    ArrowFatTk -> "'=>'"
    ArrowLeftTk -> "'<-'"
    AtTk -> "'@'"
    QuestionTk -> "'?'"
    UnderscoreTk -> "'_'"

    _ -> T.pack (show kd)

bugCombinator :: String -> a
bugCombinator msg =
  error ("Fuddle.Compiler.Parse.Internal.Combinator: " <> msg)