module Fuddle.Compiler.Parse.Internal.Markup
  ( markupExprP
  , markupElemP
  , markupAttrP
  ) where

import Control.Monad (when, void)

import Data.Text (Text)

import Fuddle.Compiler.Base.Diag ( CodeDiag(..), SeverityDiag(..) , StageDiag(..), mkDiag )
import Fuddle.Compiler.Base.Range (emptyRange)
import Fuddle.Compiler.Parse.Internal.Combinator (
            emitSyntheticTok, emitTok, expectTok, matchTok
            , withNode
  )
import Fuddle.Compiler.Parse.Internal.State ( Parser, markErr, peekTokMay)
import Fuddle.Compiler.Syntax.Kind ( SyntaxKind(..) )
import Fuddle.Compiler.Syntax.Token ( SyntheticReason(..), TokenLex(..) )


markupExprP :: Parser ()
markupExprP = withNode ExprMarkupNd markupElemP

markupElemP :: Parser ()
markupElemP =
  withNode ElemMarkupNd $ do
    expectTok LtTk
    frag0 <- matchTok GtTk
    if frag0
      then withNode FragmentMarkupNd fragmentMarkupP
      else elemMarkupP

markupAttrP :: Parser ()
markupAttrP =
  withNode AttrMarkupNd $ do
    interp0 <- matchTok InterpOpenTk
    if interp0
      then interpMarkupP
      else do
        qnameMarkupP
        eq0 <- matchTok EqualTk
        when eq0 $ do
            emitTok
            valueMarkupP


elemMarkupP :: Parser ()
elemMarkupP = do
  qnameMarkupP
  attrsMarkupP
  selfClose0 <- matchTok SlashGtTk
  if selfClose0 then
    emitTok
  else do
    expectTok GtTk
    childrenMarkupP
    expectTok LtSlashTk
    qnameMarkupP
    expectTok GtTk
  pure ()

fragmentMarkupP :: Parser ()
fragmentMarkupP = do
  expectTok GtTk
  childrenMarkupP
  expectTok LtSlashTk
  void $ expectTok GtTk


attrsMarkupP :: Parser ()
attrsMarkupP = do
  kdMay <- peekKdMay
  case kdMay of
    Nothing -> pure ()
    Just EndTk -> pure ()
    Just GtTk -> pure ()
    Just SlashGtTk -> pure ()
    _ -> do
      markupAttrP
      attrsMarkupP

childrenMarkupP :: Parser ()
childrenMarkupP = do
  kdMay <- peekKdMay
  case kdMay of
    Nothing -> pure ()
    Just EndTk -> pure ()
    Just LtSlashTk -> pure ()
    Just LtTk -> do
      markupElemP
      childrenMarkupP
    Just InterpOpenTk -> do
      interpMarkupP
      childrenMarkupP
    _ -> do
      textMarkupP
      childrenMarkupP

textMarkupP :: Parser ()
textMarkupP =
  withNode TextMarkupNd $ do
    emitTok
    textTailMarkupP

textTailMarkupP :: Parser ()
textTailMarkupP = do
  kdMay <- peekKdMay
  case kdMay of
    Nothing -> pure ()
    Just EndTk -> pure ()
    Just LtTk -> pure ()
    Just LtSlashTk -> pure ()
    Just InterpOpenTk -> pure ()
    _ -> do
      emitTok
      textTailMarkupP

interpMarkupP :: Parser ()
interpMarkupP =
  withNode InterpMarkupNd $ do
    expectTok InterpOpenTk
    rawUntilMarkupP [InterpCloseTk]
    void $ expectTok InterpCloseTk

valueMarkupP :: Parser ()
valueMarkupP = do
  kdMay <- peekKdMay
  case kdMay of
    Nothing -> missingValueMarkupP
    Just EndTk -> missingValueMarkupP
    Just InterpOpenTk -> interpMarkupP
    Just StringOpenTk -> rawStringMarkupP StringOpenTk StringCloseTk
    Just MultiStringOpenTk -> rawStringMarkupP MultiStringOpenTk MultiStringCloseTk
    Just LParenTk -> rawGroupMarkupP LParenTk RParenTk
    Just LBracketTk -> rawGroupMarkupP LBracketTk RBracketTk
    Just LBraceTk -> rawGroupMarkupP LBraceTk RBraceTk
    Just LtTk -> markupElemP
    Just kd
      | isNameMarkupKd kd -> qnameMarkupP
      | otherwise -> void emitTok

qnameMarkupP :: Parser ()
qnameMarkupP = do
  namePieceMarkupP
  qnameTailMarkupP

qnameTailMarkupP :: Parser ()
qnameTailMarkupP = do
  dot0 <- matchTok DotTk
  when dot0 $ do
      emitTok
      namePieceMarkupP
      qnameTailMarkupP

namePieceMarkupP :: Parser ()
namePieceMarkupP = do
  kdMay <- peekKdMay
  case kdMay of
    Just kd
      | isNameMarkupKd kd -> void emitTok
    _ -> do
      errMarkupP codeNameMarkup "expected markup name"
      emitSyntheticTok LowerNameTk RecoverySR

rawGroupMarkupP :: SyntaxKind -> SyntaxKind -> Parser ()
rawGroupMarkupP openKd closeKd = do
  expectTok openKd
  rawUntilMarkupP [closeKd]
  void $ expectTok closeKd

rawStringMarkupP :: SyntaxKind -> SyntaxKind -> Parser ()
rawStringMarkupP openKd closeKd = do
  expectTok openKd
  rawUntilMarkupP [closeKd]
  void $ expectTok closeKd

rawUntilMarkupP :: [SyntaxKind] -> Parser ()
rawUntilMarkupP closes0 = do
  kdMay <- peekKdMay
  case kdMay of
    Nothing -> pure ()
    Just EndTk -> pure ()
    Just kd ->
      case closes0 of
        [closeKd]
          | kd == closeKd -> pure ()
        closeKd : restKds
          | kd == closeKd -> do
              emitTok
              rawUntilMarkupP restKds
        _
          | kd == StringOpenTk -> do
              rawStringMarkupP StringOpenTk StringCloseTk
              rawUntilMarkupP closes0
          | kd == MultiStringOpenTk -> do
              rawStringMarkupP MultiStringOpenTk MultiStringCloseTk
              rawUntilMarkupP closes0
          | kd == InterpOpenTk -> do
              interpMarkupP
              rawUntilMarkupP closes0
          | Just closeKd <- pushCloseMarkupKd kd -> do
              emitTok
              rawUntilMarkupP (closeKd : closes0)
          | otherwise -> do
              emitTok
              rawUntilMarkupP closes0

missingValueMarkupP :: Parser ()
missingValueMarkupP = do
  errMarkupP codeValueMarkup "expected markup attribute value"
  emitSyntheticTok StringChunkTk RecoverySR


peekKdMay :: Parser (Maybe SyntaxKind)
peekKdMay =
  maybe Nothing (\(_, tok0) -> Just tok0.kindTL) <$> peekTokMay


isNameMarkupKd :: SyntaxKind -> Bool
isNameMarkupKd kd =
  case kd of
    LowerNameTk -> True
    UpperNameTk -> True
    _ -> False

pushCloseMarkupKd :: SyntaxKind -> Maybe SyntaxKind
pushCloseMarkupKd kd =
  case kd of
    LParenTk -> Just RParenTk
    LBracketTk -> Just RBracketTk
    LBraceTk -> Just RBraceTk
    _ -> Nothing

errMarkupP :: CodeDiag -> Text -> Parser ()
errMarkupP code0 msg0 = do
  tokMay <- peekTokMay
  let range0 =
        case tokMay of
          Nothing -> emptyRange
          Just (_, tok0) -> tok0.rangeTL
  markErr (mkDiag code0 ParseDG ErrorDS range0 msg0)

codeNameMarkup :: CodeDiag
codeNameMarkup = CodeDiag "P-Markup-Name"

codeValueMarkup :: CodeDiag
codeValueMarkup = CodeDiag "P-Markup-Value"