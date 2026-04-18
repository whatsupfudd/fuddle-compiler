{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.Parse.Internal.Pattern ( patternP )
where

import Control.Monad (unless, void, when)
import Control.Monad.State.Strict (get, put)
import Fuddle.Compiler.Parse.Internal.Combinator ( 
    emitSyntheticTok, emitTok, expectTok, matchTok, withNode
  )
import Fuddle.Compiler.Parse.Internal.State ( Parser, bumpTok, peekTokMay)
import Fuddle.Compiler.Syntax.Kind ( SyntaxKind(..), isTriviaKd )
import Fuddle.Compiler.Syntax.Token ( SyntheticReason(..), TokenLex(..))

data ParenShape =
    UnitPS
  | ParenPS
  | TuplePS
  deriving stock (Eq, Ord, Show)

patternP :: Parser ()
patternP = consPatternP

consPatternP :: Parser ()
consPatternP = do
  consB <- lookConsTailP
  if consB
    then withNode ConsPatternNd $ do
      appPatternP
      expectTok ColonColonTk
      patternP
    else appPatternP

appPatternP :: Parser ()
appPatternP = do
  kdMay <- nextKindMayP
  case kdMay of
    Just UpperNameTk -> ctorAppPatternP
    _ -> atomPatternP

atomPatternP :: Parser ()
atomPatternP = do
  kdMay <- nextKindMayP
  case kdMay of
    Just UnderscoreTk -> wildcardPatternP
    Just LowerNameTk -> varPatternP
    Just HoleNameTk -> varPatternP
    Just UpperNameTk -> ctorAtomPatternP
    Just IntTk -> literalPatternP
    Just FloatTk -> literalPatternP
    Just CharTk -> literalPatternP
    Just StringOpenTk -> literalPatternP
    Just MultiStringOpenTk -> literalPatternP
    Just LParenTk -> parenLikePatternP
    Just LBracketTk -> listPatternP
    Just LBraceTk -> recordPatternP
    _ -> missingOrErrorPatternP

wildcardPatternP :: Parser ()
wildcardPatternP =
  withNode WildcardPatternNd $
    void $ expectTok UnderscoreTk

varPatternP :: Parser ()
varPatternP =
  withNode VarPatternNd $
    takeVarNameP

ctorAtomPatternP :: Parser ()
ctorAtomPatternP =
  withNode CtorPatternNd $
    upperQNameP

ctorAppPatternP :: Parser ()
ctorAppPatternP =
  withNode CtorPatternNd $ do
    upperQNameP
    ctorArgsP

ctorArgsP :: Parser ()
ctorArgsP = do
  atomB <- atomStartP
  when atomB $ do
    atomPatternP
    ctorArgsP

literalPatternP :: Parser ()
literalPatternP = withNode LiteralPatternNd literalBodyP

literalBodyP :: Parser ()
literalBodyP = do
  kdMay <- nextKindMayP
  case kdMay of
    Just IntTk -> void $ expectTok IntTk
    Just FloatTk -> void $ expectTok FloatTk
    Just CharTk -> void $ expectTok CharTk
    Just StringOpenTk -> stringLiteralP StringOpenTk StringChunkTk StringCloseTk
    Just MultiStringOpenTk -> stringLiteralP MultiStringOpenTk MultiStringChunkTk MultiStringCloseTk
    _ -> withNode MissingNd (pure ())

stringLiteralP :: SyntaxKind -> SyntaxKind -> SyntaxKind -> Parser ()
stringLiteralP openKd chunkKd closeKd = do
  expectTok openKd
  stringChunksP chunkKd
  void $ expectTok closeKd

stringChunksP :: SyntaxKind -> Parser ()
stringChunksP chunkKd = do
  kdMay <- nextKindMayP
  case kdMay of
    Just kd | kd == chunkKd -> do
      expectTok chunkKd
      stringChunksP chunkKd
    _ -> pure ()

parenLikePatternP :: Parser ()
parenLikePatternP = do
  shape <- lookParenShapeP
  case shape of
    UnitPS -> unitPatternP
    ParenPS -> parenPatternP
    TuplePS -> tuplePatternP

unitPatternP :: Parser ()
unitPatternP =
  withNode UnitPatternNd $ do
    expectTok LParenTk
    void $ expectTok RParenTk

parenPatternP :: Parser ()
parenPatternP =
  withNode ParenPatternNd $ do
    expectTok LParenTk
    patternP
    void $ expectTok RParenTk

tuplePatternP :: Parser ()
tuplePatternP =
  withNode TuplePatternNd $ do
    expectTok LParenTk
    patternP
    expectTok CommaTk
    patternP
    tupleTailP
    void $ expectTok RParenTk

tupleTailP :: Parser ()
tupleTailP = do
  commaB <- matchTok CommaTk
  when commaB $ do
    patternP
    tupleTailP

listPatternP :: Parser ()
listPatternP =
  withNode ListPatternNd $ do
    expectTok LBracketTk
    listItemsP
    void $ expectTok RBracketTk

listItemsP :: Parser ()
listItemsP = do
  endB <- matchTok RBracketTk
  if endB
    then emitSyntheticTok RBracketTk RecoverySR
    else do
      itemB <- patternStartOrBoundaryP RBracketTk
      when itemB $ do
        patternP
        listTailP

listTailP :: Parser ()
listTailP = do
  commaB <- matchTok CommaTk
  when commaB $ do
    patternP
    listTailP

recordPatternP :: Parser ()
recordPatternP =
  withNode RecordPatternNd $ do
    expectTok LBraceTk
    recordFieldsP
    void $ expectTok RBraceTk

recordFieldsP :: Parser ()
recordFieldsP = do
  endB <- matchTok RBraceTk
  if endB then
    emitSyntheticTok RBraceTk RecoverySR
  else do
    fieldB <- patternStartOrBoundaryP RBraceTk
    when fieldB $ do
      takeFieldNameP
      recordFieldTailP

recordFieldTailP :: Parser ()
recordFieldTailP = do
  commaB <- matchTok CommaTk
  when commaB $ do
    takeFieldNameP
    recordFieldTailP

upperQNameP :: Parser ()
upperQNameP = do
  takeUpperNameP
  upperQNameTailP

upperQNameTailP :: Parser ()
upperQNameTailP = do
  dotB <- matchTok DotTk
  when dotB $ do
    takeUpperNameP
    upperQNameTailP

takeUpperNameP :: Parser ()
takeUpperNameP = do
  upperB <- matchTok UpperNameTk
  unless upperB $
    emitSyntheticTok UpperNameTk RecoverySR

takeVarNameP :: Parser ()
takeVarNameP = do
  lowerB <- matchTok LowerNameTk
  if lowerB
    then pure ()
    else do
      holeB <- matchTok HoleNameTk
      unless holeB $
        emitSyntheticTok LowerNameTk RecoverySR

takeFieldNameP :: Parser ()
takeFieldNameP = do
  lowerB <- matchTok LowerNameTk
  unless lowerB $
    emitSyntheticTok LowerNameTk RecoverySR

missingOrErrorPatternP :: Parser ()
missingOrErrorPatternP = do
  kdMay <- nextKindMayP
  case kdMay of
    Nothing -> withNode MissingNd (pure ())
    Just kd
      | isPatternStopKd kd -> withNode MissingNd (pure ())
      | otherwise -> withNode ErrorNd (void emitTok)

atomStartP :: Parser Bool
atomStartP = maybe False isAtomStartKd <$> nextKindMayP

patternStartOrBoundaryP :: SyntaxKind -> Parser Bool
patternStartOrBoundaryP closeKd = do
  kdMay <- nextKindMayP
  pure $
    case kdMay of
      Nothing -> False
      Just kd -> kd /= closeKd

nextKindMayP :: Parser (Maybe SyntaxKind)
nextKindMayP =
  maybe Nothing (\(_, tok0) -> Just tok0.kindTL) <$> peekTokMay

lookConsTailP :: Parser Bool
lookConsTailP = do
  st0 <- get
  consB <- scanConsTailP 0 0 0
  put st0
  pure consB

scanConsTailP :: Int -> Int -> Int -> Parser Bool
scanConsTailP parenN bracketN braceN = do
  kdMay <- nextKindMayP
  case kdMay of
    Nothing -> pure False
    Just kd
      | isTriviaKd kd -> void bumpTok >> scanConsTailP parenN bracketN braceN
      | topLevelB parenN bracketN braceN && kd == ColonColonTk -> pure True
      | topLevelB parenN bracketN braceN && isPatternStopKd kd -> pure False
      | kd == LParenTk -> void bumpTok >> scanConsTailP (parenN + 1) bracketN braceN
      | kd == RParenTk ->
          if parenN == 0
            then pure False
            else void bumpTok >> scanConsTailP (parenN - 1) bracketN braceN
      | kd == LBracketTk -> void bumpTok >> scanConsTailP parenN (bracketN + 1) braceN
      | kd == RBracketTk ->
          if bracketN == 0
            then pure False
            else void bumpTok >> scanConsTailP parenN (bracketN - 1) braceN
      | kd == LBraceTk -> void bumpTok >> scanConsTailP parenN bracketN (braceN + 1)
      | kd == RBraceTk ->
          if braceN == 0
            then pure False
            else void bumpTok >> scanConsTailP parenN bracketN (braceN - 1)
      | otherwise -> void bumpTok >> scanConsTailP parenN bracketN braceN

lookParenShapeP :: Parser ParenShape
lookParenShapeP = do
  st0 <- get
  void bumpTok
  shape <- scanParenShapeP 0 0 0
  put st0
  pure shape

scanParenShapeP :: Int -> Int -> Int -> Parser ParenShape
scanParenShapeP parenN bracketN braceN = do
  kdMay <- nextKindMayP
  case kdMay of
    Nothing -> pure ParenPS
    Just kd
      | isTriviaKd kd -> void bumpTok >> scanParenShapeP parenN bracketN braceN
      | topLevelB parenN bracketN braceN && kd == RParenTk -> pure UnitPS
      | topLevelB parenN bracketN braceN && kd == CommaTk -> pure TuplePS
      | topLevelB parenN bracketN braceN -> pure ParenPS
      | kd == LParenTk -> void bumpTok >> scanParenShapeP (parenN + 1) bracketN braceN
      | kd == RParenTk ->
          if parenN == 0
            then pure ParenPS
            else void bumpTok >> scanParenShapeP (parenN - 1) bracketN braceN
      | kd == LBracketTk -> void bumpTok >> scanParenShapeP parenN (bracketN + 1) braceN
      | kd == RBracketTk ->
          if bracketN == 0
            then pure ParenPS
            else void bumpTok >> scanParenShapeP parenN (bracketN - 1) braceN
      | kd == LBraceTk -> void bumpTok >> scanParenShapeP parenN bracketN (braceN + 1)
      | kd == RBraceTk ->
          if braceN == 0
            then pure ParenPS
            else void bumpTok >> scanParenShapeP parenN bracketN (braceN - 1)
      | otherwise -> void bumpTok >> scanParenShapeP parenN bracketN braceN

topLevelB :: Int -> Int -> Int -> Bool
topLevelB parenN bracketN braceN =
  parenN == 0 && bracketN == 0 && braceN == 0

isAtomStartKd :: SyntaxKind -> Bool
isAtomStartKd kd =
  case kd of
    UnderscoreTk -> True
    LowerNameTk -> True
    HoleNameTk -> True
    UpperNameTk -> True
    IntTk -> True
    FloatTk -> True
    CharTk -> True
    StringOpenTk -> True
    MultiStringOpenTk -> True
    LParenTk -> True
    LBracketTk -> True
    LBraceTk -> True
    _ -> False

isPatternStopKd :: SyntaxKind -> Bool
isPatternStopKd kd =
  case kd of
    EndTk -> True
    CommaTk -> True
    RParenTk -> True
    RBracketTk -> True
    RBraceTk -> True
    ArrowThinTk -> True
    ArrowFatTk -> True
    EqualTk -> True
    PipeTk -> True
    LayoutSepTk -> True
    LayoutCloseTk -> True
    InKwTk -> True
    OfKwTk -> True
    ThenKwTk -> True
    ElseKwTk -> True
    CatchKwTk -> True
    _ -> False