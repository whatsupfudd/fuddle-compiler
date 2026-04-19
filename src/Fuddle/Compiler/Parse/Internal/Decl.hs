module Fuddle.Compiler.Parse.Internal.Decl
  ( topDeclP
  , valueDeclP
  , typeDeclP
  , aliasDeclP
  , effectDeclP
  , foreignDeclP
  , fixityDeclP
  ) where

import Control.Monad (void, when)
import Control.Applicative ((<|>))

import Fuddle.Compiler.Parse.Internal.Combinator (expectTok, matchTok, withNode)
import Fuddle.Compiler.Parse.Internal.Expr (exprP)
import Fuddle.Compiler.Parse.Internal.Foreign (foreignBlockP, foreignExprP)
import Fuddle.Compiler.Parse.Internal.Pattern (patternP)
import Fuddle.Compiler.Parse.Internal.Recover (recoverUntil, topAnchors, withRecover)
import Fuddle.Compiler.Parse.Internal.State (Parser, peekTokMay, lookAheadP)
import Fuddle.Compiler.Parse.Internal.Type (typeP, typeRowP)
import Fuddle.Compiler.Syntax.Kind
import Fuddle.Compiler.Syntax.Token (TokenLex(..))


topDeclP :: Parser ()
topDeclP =
  withRecover topAnchors $ do
    kdMay <- nextKdMayP
    case kdMay of
      Nothing -> pure ()
      Just TypeKwTk -> do
        aliasHd <- aliasHeadP
        if aliasHd then aliasDeclP else typeDeclP
      Just EffectKwTk -> effectDeclP
      Just ForeignKwTk -> foreignDeclP
      Just InfixKwTk -> fixityDeclP
      Just RegionKwTk -> regionDeclP
      Just AnchorKwTk -> anchorDeclP
      Just CellKwTk -> cellDeclP
      Just NativeKwTk -> nativeDeclP
      Just ThreadKwTk -> do
        sigHd <- threadSigHeadP
        if sigHd then threadSigP else threadDeclP
      Just LowerNameTk -> do
        sigHd <- valueSigHeadP
        if sigHd then valueSigP else valueDeclP
      _ -> recoverUntil topAnchors

valueDeclP :: Parser ()
valueDeclP =
  withNode ValueDeclNd $ do
    expectTok LowerNameTk
    paramsValueP
    expectTok EqualTk
    exprP

typeDeclP :: Parser ()
typeDeclP =
  withNode TypeDeclNd $ do
    expectTok TypeKwTk
    expectTok UpperNameTk
    varsTypeP
    expectTok EqualTk
    ctorDeclP
    ctorsMoreP

aliasDeclP :: Parser ()
aliasDeclP =
  withNode AliasDeclNd $ do
    expectTok TypeKwTk
    expectTok AliasKwTk
    expectTok UpperNameTk
    varsTypeP
    expectTok EqualTk
    typeP

effectDeclP :: Parser ()
effectDeclP =
  withNode EffectDeclNd $ do
    expectTok EffectKwTk
    expectTok UpperNameTk
    varsTypeP
    expectTok EqualTk
    typeRowP

foreignDeclP :: Parser ()
foreignDeclP =
  withNode ForeignDeclNd $ do
    expectTok ForeignKwTk
    expectTok LowerNameTk
    colonHas <- matchTok ColonTk
    if colonHas
      then do
        typeP
        equalHas <- matchTok EqualTk
        if equalHas then bodyForeignP else pure ()
      else do
        expectTok EqualTk
        bodyForeignP

fixityDeclP :: Parser ()
fixityDeclP =
  withNode FixityDeclNd $ do
    expectTok InfixKwTk
    assocFixityP
    expectTok IntTk
    void $ expectTok OperatorTk

valueSigP :: Parser ()
valueSigP =
  withNode ValueSigNd $ do
    expectTok LowerNameTk
    expectTok ColonTk
    typeP

ctorDeclP :: Parser ()
ctorDeclP =
  withNode CtorDeclNd $ do
    expectTok UpperNameTk
    argsCtorP

regionDeclP :: Parser ()
regionDeclP =
  withNode RegionDeclNd $ do
    expectTok RegionKwTk
    expectTok UpperNameTk
    expectTok ColonTk
    refUpperP

anchorDeclP :: Parser ()
anchorDeclP =
  withNode AnchorDeclNd $ do
    expectTok AnchorKwTk
    expectTok LowerNameTk
    expectTok ColonTk
    typeP

cellDeclP :: Parser ()
cellDeclP =
  withNode CellDeclNd $ do
    expectTok CellKwTk
    expectTok LowerNameTk
    expectTok ColonTk
    typeP
    expectTok EqualTk
    exprP

nativeDeclP :: Parser ()
nativeDeclP =
  withNode NativeDeclNd $ do
    expectTok NativeKwTk
    expectTok LowerNameTk
    expectTok ColonTk
    typeP

threadSigP :: Parser ()
threadSigP =
  withNode ThreadSigNd $ do
    expectTok ThreadKwTk
    expectTok LowerNameTk
    expectTok ColonTk
    typeP

threadDeclP :: Parser ()
threadDeclP =
  withNode ThreadDeclNd $ do
    expectTok ThreadKwTk
    expectTok LowerNameTk
    paramsThreadP
    expectTok EqualTk
    exprP

bodyForeignP :: Parser ()
bodyForeignP = foreignBlockP <|> foreignExprP

assocFixityP :: Parser ()
assocFixityP = do
  leftHas <- matchTok LeftKwTk
  if leftHas
    then pure ()
    else do
      rightHas <- matchTok RightKwTk
      if rightHas then pure () else void $ expectTok LeftKwTk

varsTypeP :: Parser ()
varsTypeP = do
  kdMay <- nextKdMayP
  case kdMay of
    Just LowerNameTk -> expectTok LowerNameTk >> varsTypeP
    _ -> pure ()

ctorsMoreP :: Parser ()
ctorsMoreP = do
  pipeHas <- matchTok PipeTk
  if pipeHas
    then ctorDeclP >> ctorsMoreP
    else pure ()

argsCtorP :: Parser ()
argsCtorP = do
  kdMay <- nextKdMayP
  case kdMay of
    Just kd | startsArgTypeKd kd -> typeP >> argsCtorP
    _ -> pure ()

paramsValueP :: Parser ()
paramsValueP = do
  kdMay <- nextKdMayP
  case kdMay of
    Just EqualTk -> pure ()
    Just kd | startsParamKd kd -> patternP >> paramsValueP
    _ -> pure ()

paramsThreadP :: Parser ()
paramsThreadP = do
  kdMay <- nextKdMayP
  case kdMay of
    Just EqualTk -> pure ()
    Just kd | startsParamKd kd -> patternP >> paramsThreadP
    _ -> pure ()

refUpperP :: Parser ()
refUpperP = do
  expectTok UpperNameTk
  refUpperTailP

refUpperTailP :: Parser ()
refUpperTailP = do
  dotHas <- matchTok DotTk
  when dotHas $ do
    expectTok UpperNameTk
    refUpperTailP


aliasHeadP :: Parser Bool
aliasHeadP =
  lookAheadP $ do
    typeHas <- matchTok TypeKwTk
    if typeHas then matchTok AliasKwTk else pure False

valueSigHeadP :: Parser Bool
valueSigHeadP =
  lookAheadP $ do
    nameHas <- matchTok LowerNameTk
    if nameHas then matchTok ColonTk else pure False

threadSigHeadP :: Parser Bool
threadSigHeadP =
  lookAheadP $ do
    threadHas <- matchTok ThreadKwTk
    if threadHas
      then do
        nameHas <- matchTok LowerNameTk
        if nameHas then matchTok ColonTk else pure False
      else pure False

nextKdMayP :: Parser (Maybe SyntaxKind)
nextKdMayP = fmap (\(_, tok) -> tok.kindTL) <$> peekTokMay

startsArgTypeKd :: SyntaxKind -> Bool
startsArgTypeKd kd =
  case kd of
    LowerNameTk -> True
    UpperNameTk -> True
    LParenTk -> True
    LBraceTk -> True
    _ -> False

startsParamKd :: SyntaxKind -> Bool
startsParamKd kd =
  case kd of
    UnderscoreTk -> True
    LowerNameTk -> True
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