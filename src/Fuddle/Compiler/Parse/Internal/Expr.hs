module Fuddle.Compiler.Parse.Internal.Expr
  ( exprP
  , ifExprP
  , caseExprP
  , letExprP
  , doExprP
  , lambdaExprP
  , applyExprP
  , binaryExprP
  ) where

import Control.Monad (unless, when, void)
import Control.Applicative ((<|>), many, some)

import Text.Megaparsec (try)

import Fuddle.Compiler.Parse.Internal.Combinator (
    acceptTok, emitTok, expectTok, matchTok, withNode
  )
import Fuddle.Compiler.Parse.Internal.Pattern (patternP)
import Fuddle.Compiler.Parse.Internal.State (Parser (..), peekTokMay, softFail, tryP)
import Fuddle.Compiler.Parse.Internal.Type (typeP)
import Fuddle.Compiler.Syntax.Kind (SyntaxKind(..))
import Fuddle.Compiler.Syntax.Token (TokenLex(..))

exprP :: Parser ()
exprP = do
  kdMay <- peekKdMay
  case kdMay of
    Just IfKwTk -> ifExprP
    Just CaseKwTk -> caseExprP
    Just LetKwTk -> letExprP
    Just BackslashTk -> lambdaExprP
    Just DoKwTk -> doExprP
    _ -> binaryExprP

ifExprP :: Parser ()
ifExprP =
  withNode IfExprNd $ do
    expectTok IfKwTk
    exprP
    expectTok ThenKwTk
    exprP
    expectTok ElseKwTk
    exprP

caseExprP :: Parser ()
caseExprP =
  withNode CaseExprNd $ do
    expectTok CaseKwTk
    exprP
    expectTok OfKwTk
    blockSomeP branchCaseP

letExprP :: Parser ()
letExprP =
  withNode LetExprNd $ do
    expectTok LetKwTk
    blockSomeP declLetP
    expectTok InKwTk
    exprP

doExprP :: Parser ()
doExprP =
  withNode DoExprNd $ do
    expectTok DoKwTk
    blockSomeP stmtDoP
    clauseCatchMayP

lambdaExprP :: Parser ()
lambdaExprP =
  withNode LambdaExprNd $ do
    expectTok BackslashTk
    some patternP
    expectTok ArrowThinTk
    exprP

binaryExprP :: Parser ()
binaryExprP = tryP consExprP <|> tryP opExprP <|> applyExprP

applyExprP :: Parser ()
applyExprP = tryP applyChainP <|> anchoredExprP

branchCaseP :: Parser ()
branchCaseP =
  withNode BranchCaseNd $ do
    patternP
    expectTok ArrowThinTk
    exprP

declLetP :: Parser ()
declLetP = sigLetP <|> bindLetP

sigLetP :: Parser ()
sigLetP =
  withNode ValueSigNd $ do
    expectTok LowerNameTk
    expectTok ColonTk
    withNode TypeAnnNd typeP

bindLetP :: Parser ()
bindLetP =
  withNode BindingLetNd $ do
    expectTok LowerNameTk
    many patternP
    expectTok EqualTk
    exprP

stmtDoP :: Parser ()
stmtDoP = do
  kdMay <- peekKdMay
  case kdMay of
    Just LetKwTk -> letDoP
    _ -> tryP bindDoP <|> plainDoP

letDoP :: Parser ()
letDoP =
  withNode LetDoNd $ do
    expectTok LetKwTk
    blockSomeP declLetP

bindDoP :: Parser ()
bindDoP =
  withNode BindDoNd $ do
    patternP
    expectTok ArrowLeftTk
    exprP

plainDoP :: Parser ()
plainDoP = withNode StmtDoNd exprP

clauseCatchMayP :: Parser ()
clauseCatchMayP = do
  kdMay <- peekKdMay
  when (kdMay == Just CatchKwTk) clauseCatchP

clauseCatchP :: Parser ()
clauseCatchP =
  withNode ClauseCatchNd $ do
    expectTok CatchKwTk
    kdMay <- peekKdMay
    case kdMay of
      Just LayoutOpenTk -> blockSomeP armCatchP
      _ -> armCatchP

armCatchP :: Parser ()
armCatchP =
  withNode ArmCatchNd $ do
    patternP
    expectTok ArrowThinTk
    exprP

consExprP :: Parser ()
consExprP =
  withNode BinaryExprNd $ do
    opExprTailP
    some $ do
      expectTok ColonColonTk
      opExprTailP
    pure ()

opExprP :: Parser ()
opExprP =
  withNode BinaryExprNd $ do
    applyExprP
    some $ do
      expectTok OperatorTk
      applyExprP
    pure ()

opExprTailP :: Parser ()
opExprTailP = tryP opExprP <|> applyExprP

applyChainP :: Parser ()
applyChainP =
  withNode ApplyExprNd $ do
    anchoredExprP
    some anchoredExprP
    pure ()

anchoredExprP :: Parser ()
anchoredExprP = tryP exprMarkupP <|> postfixExprP

exprMarkupP :: Parser ()
exprMarkupP =
  withNode ExprMarkupNd $ do
    postfixExprP
    expectTok AtTk
    anchorMarkupArgP
    recordExprPlainP
    listExprP
    listExprP


anchorMarkupArgP :: Parser ()
anchorMarkupArgP = tryP parenExprP <|> nameExprP

postfixExprP :: Parser ()
postfixExprP = tryP accessExprP <|> atomExprP

accessExprP :: Parser ()
accessExprP =
  withNode AccessExprNd $ do
    atomPlainExprP
    some fieldAccessP
    pure ()

fieldAccessP :: Parser ()
fieldAccessP = do
  expectTok DotTk
  void $ expectTok LowerNameTk

atomExprP :: Parser ()
atomExprP = accessorExprP <|> atomPlainExprP

atomPlainExprP :: Parser ()
atomPlainExprP = do
  kdMay <- peekKdMay
  case kdMay of
    Just HoleNameTk -> holeExprP
    Just LowerNameTk -> nameExprP
    Just UpperNameTk -> nameExprP
    Just IntTk -> literalExprP
    Just FloatTk -> literalExprP
    Just CharTk -> literalExprP
    Just StringOpenTk -> literalExprP
    Just MultiStringOpenTk -> literalExprP
    Just LParenTk -> parenLikeExprP
    Just LBracketTk -> listExprP
    Just LBraceTk -> recordExprP
    _ -> fallbackExprP

nameExprP :: Parser ()
nameExprP =
  withNode NameExprNd $ do
    kdMay <- peekKdMay
    case kdMay of
      Just LowerNameTk -> void $ expectTok LowerNameTk
      Just UpperNameTk -> upperQNameOrCtorP
      _ -> fallbackExprLeafP

upperQNameOrCtorP :: Parser ()
upperQNameOrCtorP = do
  expectTok UpperNameTk
  upperQNameTailP

upperQNameTailP :: Parser ()
upperQNameTailP = do
  hasDot <- matchTok DotTk
  when hasDot $ do
    kdMay <- peekKdMay
    case kdMay of
      Just UpperNameTk -> expectTok UpperNameTk >> upperQNameTailP
      Just LowerNameTk -> void $ expectTok LowerNameTk
      _ -> void $ expectTok LowerNameTk

holeExprP :: Parser ()
holeExprP = withNode HoleExprNd $ void $ expectTok HoleNameTk

literalExprP :: Parser ()
literalExprP =
  withNode LiteralExprNd $ do
    kdMay <- peekKdMay
    case kdMay of
      Just IntTk -> void emitTok
      Just FloatTk -> void emitTok
      Just CharTk -> void emitTok
      Just StringOpenTk -> stringExprP StringOpenTk StringChunkTk StringCloseTk
      Just MultiStringOpenTk -> stringExprP MultiStringOpenTk MultiStringChunkTk MultiStringCloseTk
      _ -> fallbackExprLeafP

stringExprP :: SyntaxKind -> SyntaxKind -> SyntaxKind -> Parser ()
stringExprP openKd chunkKd closeKd = do
  expectTok openKd
  stringItemsP chunkKd closeKd
  void $ expectTok closeKd

stringItemsP :: SyntaxKind -> SyntaxKind -> Parser ()
stringItemsP chunkKd closeKd = do
  kdMay <- peekKdMay
  case kdMay of
    Just kd | kd == closeKd -> pure ()
    Just kd | kd == chunkKd -> emitTok >> stringItemsP chunkKd closeKd
    Just InterpOpenTk -> do
      expectTok InterpOpenTk
      exprP
      expectTok InterpCloseTk
      stringItemsP chunkKd closeKd
    _ -> pure ()

accessorExprP :: Parser ()
accessorExprP =
  withNode AccessorExprNd $ do
    expectTok DotTk
    void $ expectTok LowerNameTk


parenLikeExprP :: Parser ()
parenLikeExprP = 
  -- try unitExprP <|> try tupleExprP <|> parenExprP
  withNode ParenExprNd $ do
    expectTok LParenTk
    hasClose <- matchTok RParenTk
    if hasClose
      then pure ()  -- unit
      else do
        exprP
        hasComma <- matchTok CommaTk
        if hasComma
          then do
            exprP
            tupleTailP
            void $ expectTok RParenTk
          else do
            void $ expectTok RParenTk


unitExprP :: Parser ()
unitExprP =
  withNode UnitExprNd $ do
    expectTok LParenTk
    void $ expectTok RParenTk

parenExprP :: Parser ()
parenExprP =
  withNode ParenExprNd $ do
    expectTok LParenTk
    exprP
    void $ expectTok RParenTk

tupleExprP :: Parser ()
tupleExprP =
  withNode TupleExprNd $ do
    expectTok LParenTk
    exprP
    expectTok CommaTk
    exprP
    tupleTailP
    void $ expectTok RParenTk

tupleTailP :: Parser ()
tupleTailP = do
  hasComma <- matchTok CommaTk
  when hasComma $ do
    exprP
    tupleTailP

listExprP :: Parser ()
listExprP =
  withNode ListExprNd $ do
    expectTok LBracketTk
    hasClose <- matchTok RBracketTk
    unless hasClose $ do
      exprP
      commaManyP exprP
      void $ expectTok RBracketTk

recordExprP :: Parser ()
recordExprP = recordUpdateExprP <|> recordExprPlainP

recordExprPlainP :: Parser ()
recordExprPlainP =
  withNode RecordExprNd $ do
    expectTok LBraceTk
    hasClose <- matchTok RBraceTk
    unless hasClose $ do
      fieldRecordP
      commaManyP fieldRecordP
      void $ expectTok RBraceTk

recordUpdateExprP :: Parser ()
recordUpdateExprP =
  withNode UpdateRecordExprNd $ do
    acceptTok LBraceTk
    lowerQNameSpecP
    acceptTok PipeTk
    fieldRecordP
    commaManyP fieldRecordP
    void $ expectTok RBraceTk

fieldRecordP :: Parser ()
fieldRecordP =
  withNode FieldRecordNd $ do
    expectTok LowerNameTk
    expectTok EqualTk
    exprP

lowerQNameP :: Parser ()
lowerQNameP = do
  kdMay <- peekKdMay
  case kdMay of
    Just LowerNameTk -> void $ expectTok LowerNameTk
    Just UpperNameTk -> do
      expectTok UpperNameTk
      expectTok DotTk
      lowerQNameTailP
    _ -> void $ expectTok LowerNameTk


lowerQNameSpecP :: Parser ()
lowerQNameSpecP = do
  kdMay <- peekKdMay
  case kdMay of
    Just LowerNameTk -> acceptTok LowerNameTk
    Just UpperNameTk -> do
      acceptTok UpperNameTk
      acceptTok DotTk
      lowerQNameTailSpecP
    _ -> softFail

lowerQNameTailSpecP :: Parser ()
lowerQNameTailSpecP = do
  kdMay <- peekKdMay
  case kdMay of
    Just UpperNameTk -> do
      acceptTok UpperNameTk
      acceptTok DotTk
      lowerQNameTailSpecP
    Just LowerNameTk ->
      acceptTok LowerNameTk
    _ -> softFail

lowerQNameTailP :: Parser ()
lowerQNameTailP = do
  kdMay <- peekKdMay
  case kdMay of
    Just UpperNameTk -> do
      expectTok UpperNameTk
      expectTok DotTk
      lowerQNameTailP
    _ -> void $ expectTok LowerNameTk

blockSomeP :: Parser () -> Parser ()
blockSomeP itemP = do
  expectTok LayoutOpenTk
  itemP
  blockTailP itemP
  void $ expectTok LayoutCloseTk

blockTailP :: Parser () -> Parser ()
blockTailP itemP = do
  hasSep <- matchTok LayoutSepTk
  when hasSep $ do
    itemP
    blockTailP itemP

commaManyP :: Parser () -> Parser ()
commaManyP itemP = do
  hasComma <- matchTok CommaTk
  when hasComma $ do
    itemP
    commaManyP itemP

fallbackExprP :: Parser ()
fallbackExprP = do
  kdMay <- peekKdMay
  case kdMay of
    Nothing -> withNode MissingNd (pure ())
    Just kd | endsExprKd kd -> withNode MissingNd (pure ())
    Just _ -> withNode ErrorNd (void emitTok)

fallbackExprLeafP :: Parser ()
fallbackExprLeafP = fallbackExprP

peekKdMay :: Parser (Maybe SyntaxKind)
peekKdMay = fmap (fmap (\(_, tok) -> tok.kindTL)) peekTokMay

endsExprKd :: SyntaxKind -> Bool
endsExprKd kd =
  case kd of
    EndTk -> True
    RParenTk -> True
    RBracketTk -> True
    RBraceTk -> True
    CommaTk -> True
    LayoutSepTk -> True
    LayoutCloseTk -> True
    ThenKwTk -> True
    ElseKwTk -> True
    OfKwTk -> True
    InKwTk -> True
    CatchKwTk -> True
    ArrowThinTk -> True
    ArrowLeftTk -> True
    EqualTk -> True
    PipeTk -> True
    _ -> False