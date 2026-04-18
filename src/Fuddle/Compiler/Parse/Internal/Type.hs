module Fuddle.Compiler.Parse.Internal.Type
  ( typeP
  , typeAnnP
  , typeRowP
  ) where

import Control.Monad (unless, when, void)
import Control.Monad.State.Strict (get, put)
import qualified Data.Text as T
import Fuddle.Compiler.Base.Diag (
    CodeDiag(..), SeverityDiag(..), StageDiag(..), mkDiag
  )
import Fuddle.Compiler.Base.Range (Range, emptyRange)
import Fuddle.Compiler.Parse.Internal.Combinator (
    emitSyntheticTok, emitTok, withNode
  )
import Fuddle.Compiler.Parse.Internal.State ( Parser, bumpTok, markErr, peekTokMay )
import Fuddle.Compiler.Syntax.Kind ( SyntaxKind(..) )
import Fuddle.Compiler.Syntax.Token ( SyntheticReason(..), TokenLex(..), TokenFlags(..) )

typeP :: Parser ()
typeP = typeArrowP

typeAnnP :: Parser ()
typeAnnP =
  withNode TypeAnnNd $ do
    lowerNameBareP "expected a lower-case name before ':' in a type annotation"
    expectTokP ColonTk "expected ':' in a type annotation"
    typeP

typeRowP :: Parser ()
typeRowP =
  withNode RowEffectNd $ do
    expectTokP LtTk "expected '<' to start an effect row"
    done <- atTokP GtTk
    unless done $ do
      effectItemP
      effectItemsTailP
    expectTokP GtTk "expected '>' to end an effect row"

typeArrowP :: Parser ()
typeArrowP = do
  arrow <- lookaheadP $ do
    typeEffP
    atTokP ArrowThinTk
  if arrow
    then withNode TypeArrowNd $ do
      typeEffP
      expectTokP ArrowThinTk "expected '->' in a function type"
      typeArrowP
    else typeEffP

typeEffP :: Parser ()
typeEffP = do
  eff <- lookaheadP $ do
    typeAppP
    atTokP BangTk
  if eff
    then withNode TypeEffectNd $ do
      typeAppP
      expectTokP BangTk "expected '!' before an effect row"
      typeRowP
    else typeAppP

typeAppP :: Parser ()
typeAppP = do
  app <- lookaheadP $ do
    typeAtomP
    startsTypeAtomP
  if app
    then withNode TypeApplyNd $ do
      typeAtomP
      appTailP
    else typeAtomP

appTailP :: Parser ()
appTailP = do
  more <- startsTypeAtomP
  when more $ do
    typeAtomP
    appTailP

typeAtomP :: Parser ()
typeAtomP = do
  kdMay <- tokKindMayP
  case kdMay of
    Just LowerNameTk -> typeVarP
    Just UpperNameTk -> typeCtorP
    Just LParenTk -> parenTypeP
    Just LBraceTk -> recordTypeP
    _ -> unexpectedTypeAtomP

typeVarP :: Parser ()
typeVarP =
  withNode TypeVarNd $
    lowerNameBareP "expected a type variable"

typeCtorP :: Parser ()
typeCtorP =
  withNode TypeCtorNd $ do
    upperSegP "expected a type constructor"
    ctorSegsTailP

ctorSegsTailP :: Parser ()
ctorSegsTailP = do
  hasDot <- acceptTokP DotTk
  when hasDot $ do
    upperSegP "expected an upper-case path segment after '.' in a type constructor"
    ctorSegsTailP

parenTypeP :: Parser ()
parenTypeP = do
  shape <- parenShapeP
  case shape of
    UnitPS ->
      withNode TypeUnitNd $ do
        expectTokP LParenTk "expected '(' to start unit type"
        expectTokP RParenTk "expected ')' to finish unit type"

    ParenPS ->
      withNode TypeParenNd $ do
        expectTokP LParenTk "expected '(' to start parenthesized type"
        typeP
        expectTokP RParenTk "expected ')' to finish parenthesized type"

    TuplePS ->
      withNode TypeTupleNd $ do
        expectTokP LParenTk "expected '(' to start tuple type"
        typeP
        expectTokP CommaTk "expected ',' in tuple type"
        typeP
        tupleTailP
        expectTokP RParenTk "expected ')' to finish tuple type"

tupleTailP :: Parser ()
tupleTailP = do
  hasComma <- acceptTokP CommaTk
  when hasComma $ do
    done <- atTokP RParenTk
    unless done typeP
    tupleTailP

recordTypeP :: Parser ()
recordTypeP =
  withNode TypeRecordNd $ do
    expectTokP LBraceTk "expected '{' to start record type"
    done <- atTokP RBraceTk
    unless done $ do
      rowOnly <- atTokP PipeTk
      unless rowOnly $ do
        typeFieldP
        fieldTailP
      hasRow <- atTokP PipeTk
      when hasRow typeRecordRowP
    expectTokP RBraceTk "expected '}' to finish record type"

typeFieldP :: Parser ()
typeFieldP =
  withNode TypeFieldNd $ do
    lowerNameBareP "expected a lower-case field name in a record type"
    expectTokP ColonTk "expected ':' after a field name in a record type"
    typeP

fieldTailP :: Parser ()
fieldTailP = do
  hasComma <- acceptTokP CommaTk
  when hasComma $ do
    done <- anyTokP [RBraceTk, PipeTk]
    unless done typeFieldP
    fieldTailP

typeRecordRowP :: Parser ()
typeRecordRowP =
  withNode TypeRowNd $ do
    expectTokP PipeTk "expected '|' before the row variable in a record type"
    lowerNameBareP "expected a lower-case row variable after '|' in a record type"

effectItemP :: Parser ()
effectItemP = do
  failLike <- failItemShapeP
  if failLike
    then withNode FailEffectNd $ do
      lowerNameBareP "expected an effect label before the failing type"
      typeP
    else withNode ItemEffectNd $
      lowerNameBareP "expected an effect label in an effect row"

effectItemsTailP :: Parser ()
effectItemsTailP = do
  hasComma <- acceptTokP CommaTk
  when hasComma $ do
    done <- atTokP GtTk
    unless done effectItemP
    effectItemsTailP

failItemShapeP :: Parser Bool
failItemShapeP =
  lookaheadP $ do
    hasLabel <- acceptTokP LowerNameTk
    if not hasLabel
      then pure False
      else startsTypeAtomP

lowerNameBareP :: T.Text -> Parser ()
lowerNameBareP msg = expectNameP LowerNameTk msg

upperSegP :: T.Text -> Parser ()
upperSegP msg = expectNameP UpperNameTk msg

expectNameP :: SyntaxKind -> T.Text -> Parser ()
expectNameP kd msg = do
  ok <- acceptTokP kd
  unless ok $ do
    markDiagP "P-TYPE-NAME" msg
    consumeUnexpectedP
    emitSyntheticTok kd RecoverySR

unexpectedTypeAtomP :: Parser ()
unexpectedTypeAtomP = do
  markDiagP "P-TYPE-ATOM" "expected a type"
  tokMay <- peekTokMay
  case tokMay of
    Just (_, tok) | not (isTypeStopKd tok.kindTL) ->
      withNode ErrorNd $ do
        emitTok
        void bumpTok
    _ ->
      withNode MissingNd (pure ())

parenShapeP :: Parser ParenShape
parenShapeP =
  lookaheadP $ do
    _ <- acceptTokP LParenTk
    unit <- atTokP RParenTk
    if unit
      then pure UnitPS
      else do
        typeP
        tuple <- atTokP CommaTk
        pure (if tuple then TuplePS else ParenPS)

acceptTokP :: SyntaxKind -> Parser Bool
acceptTokP kd = do
  tokMay <- peekTokMay
  case tokMay of
    Just (_, tok) | tok.kindTL == kd -> do
      emitTok
      bumpTok
      pure True
    _ ->
      pure False

expectTokP :: SyntaxKind -> T.Text -> Parser ()
expectTokP kd msg = do
  ok <- acceptTokP kd
  unless ok $ do
    markDiagP "P-TYPE-TOK" msg
    emitSyntheticTok kd RecoverySR

atTokP :: SyntaxKind -> Parser Bool
atTokP kd = do
  tokMay <- peekTokMay
  pure $
    case tokMay of
      Just (_, tok) -> tok.kindTL == kd
      Nothing -> False

anyTokP :: [SyntaxKind] -> Parser Bool
anyTokP kds = do
  kdMay <- tokKindMayP
  pure $
    case kdMay of
      Just kd -> kd `elem` kds
      Nothing -> False

startsTypeAtomP :: Parser Bool
startsTypeAtomP = do
  kdMay <- tokKindMayP
  pure $
    case kdMay of
      Just kd -> startsTypeAtomKd kd
      Nothing -> False

startsTypeAtomKd :: SyntaxKind -> Bool
startsTypeAtomKd kd =
  case kd of
    LowerNameTk -> True
    UpperNameTk -> True
    LParenTk -> True
    LBraceTk -> True
    _ -> False

isTypeStopKd :: SyntaxKind -> Bool
isTypeStopKd kd =
  case kd of
    EndTk -> True
    LayoutSepTk -> True
    LayoutCloseTk -> True
    RParenTk -> True
    RBracketTk -> True
    RBraceTk -> True
    CommaTk -> True
    ColonTk -> True
    PipeTk -> True
    ArrowThinTk -> True
    BangTk -> True
    GtTk -> True
    EqualTk -> True
    _ -> False

consumeUnexpectedP :: Parser ()
consumeUnexpectedP = do
  tokMay <- peekTokMay
  case tokMay of
    Just (_, tok) | not (isTypeStopKd tok.kindTL) ->
      withNode ErrorNd $ do
        emitTok
        void bumpTok
    _ ->
      pure ()

tokKindMayP :: Parser (Maybe SyntaxKind)
tokKindMayP =
  fmap (\(_, tok0) -> tok0.kindTL) <$> peekTokMay

currentRangeP :: Parser Range
currentRangeP = do
  tokMay <- peekTokMay
  pure $
    case tokMay of
      Just (_, tok) -> tok.rangeTL
      Nothing -> emptyRange

markDiagP :: T.Text -> T.Text -> Parser ()
markDiagP code msg = do
  range0 <- currentRangeP
  gotTxt <- gotTokTxtP
  let msg0 =
        case gotTxt of
          Just txt -> msg <> " (found " <> txt <> ")"
          Nothing -> msg
  markErr (mkDiag (CodeDiag code) ParseDG ErrorDS range0 msg0)

gotTokTxtP :: Parser (Maybe T.Text)
gotTokTxtP = do
  tokMay <- peekTokMay
  pure $ case tokMay of
    Just (_, tok) -> Just (T.pack (show tok.kindTL))
    Nothing -> Nothing

lookaheadP :: Parser a -> Parser a
lookaheadP act = do
  st0 <- get
  res <- act
  put st0
  pure res

data ParenShape
  = UnitPS
  | ParenPS
  | TuplePS