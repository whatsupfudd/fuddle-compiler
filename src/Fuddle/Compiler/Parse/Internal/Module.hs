module Fuddle.Compiler.Parse.Internal.Module
  ( sourceFileP
  , moduleDeclP
  , importDeclP
  ) where

import Control.Monad (unless, when, void)
import Fuddle.Compiler.Parse.Internal.Combinator
  ( expectTok
  , matchTok
  , withNode
  )
import Fuddle.Compiler.Parse.Internal.Decl (topDeclP)
import Fuddle.Compiler.Parse.Internal.Recover
  ( recoverUntil
  , topAnchors
  , withRecover
  )
import Fuddle.Compiler.Parse.Internal.State (Parser, peekTokMay)
import Fuddle.Compiler.Syntax.Kind
import Fuddle.Compiler.Syntax.Token (TokenLex(kindTL))

sourceFileP :: Parser ()
sourceFileP =
  withNode SourceFileNd $ do
    skipTopNoiseP
    moduleDeclP
    topItemsP

moduleDeclP :: Parser ()
moduleDeclP =
  withRecover topAnchors $
    withNode ModuleDeclNd $ do
      expectTok ModuleKwTk
      moduleHeadP
      expectTok ExposingKwTk
      exportListP

importDeclP :: Parser ()
importDeclP =
  withRecover topAnchors $
    withNode ImportDeclNd $ do
      expectTok ImportKwTk
      upperPathP
      importAliasMayP
      importItemsMayP

topItemsP :: Parser ()
topItemsP = do
  skipTopNoiseP
  done <- atEndP
  unless done $ do
    parsed <- topItemP
    unless parsed (recoverUntil topAnchors)
    topItemsP

topItemP :: Parser Bool
topItemP = do
  kdMay <- peekKdMay
  case kdMay of
    Just ImportKwTk -> importDeclP >> pure True
    Just kd | startsTopDeclKd kd -> topDeclP >> pure True
    _ -> pure False

moduleHeadP :: Parser ()
moduleHeadP =
  withNode ModuleHeadNd upperPathP

upperPathP :: Parser ()
upperPathP = do
  expectTok UpperNameTk
  upperPathRestP

upperPathRestP :: Parser ()
upperPathRestP = do
  hasDot <- matchTok DotTk
  when hasDot $ do
    expectTok UpperNameTk
    upperPathRestP

importAliasMayP :: Parser ()
importAliasMayP = do
  hasAlias <- atTokP AsKwTk
  when hasAlias $
    withNode ImportAliasNd $ do
      expectTok AsKwTk
      void $ expectTok UpperNameTk

importItemsMayP :: Parser ()
importItemsMayP = do
  hasExposing <- atTokP ExposingKwTk
  when hasExposing $
    withNode ImportItemsNd $ do
      expectTok ExposingKwTk
      parenItemsP ImportItemNd exposeItemBodyP

exportListP :: Parser ()
exportListP =
  withNode ExportListNd $
    parenItemsP ExportItemNd exposeItemBodyP

parenItemsP :: SyntaxKind -> Parser () -> Parser ()
parenItemsP itemKd itemP = do
  expectTok LParenTk
  empty <- atTokP RParenTk
  if empty
    then pure ()
    else do
      allMay <- matchTok DotDotTk
      unless allMay $ do
        withNode itemKd itemP
        parenItemsRestP itemKd itemP
  void $ expectTok RParenTk

parenItemsRestP :: SyntaxKind -> Parser () -> Parser ()
parenItemsRestP itemKd itemP = do
  hasComma <- matchTok CommaTk
  when hasComma $ do
    done <- atTokP RParenTk
    unless done $ do
      withNode itemKd itemP
      parenItemsRestP itemKd itemP

exposeItemBodyP :: Parser ()
exposeItemBodyP = do
  kdMay <- peekKdMay
  case kdMay of
    Just UpperNameTk -> do
      expectTok UpperNameTk
      ctorExposeMayP
    _ -> void $ expectTok LowerNameTk

ctorExposeMayP :: Parser ()
ctorExposeMayP = do
  hasParen <- atTokP LParenTk
  when hasParen $ do
    expectTok LParenTk
    expectTok DotDotTk
    void $ expectTok RParenTk

skipTopNoiseP :: Parser ()
skipTopNoiseP = do
  sawSep <- matchTok LayoutSepTk
  sawOpen <- matchTok LayoutOpenTk
  sawClose <- matchTok LayoutCloseTk
  when (sawSep || sawOpen || sawClose) skipTopNoiseP

atEndP :: Parser Bool
atEndP = do
  kdMay <- peekKdMay
  pure $
    case kdMay of
      Nothing -> True
      Just EndTk -> True
      _ -> False

atTokP :: SyntaxKind -> Parser Bool
atTokP kd = do
  kdMay <- peekKdMay
  pure (kdMay == Just kd)

peekKdMay :: Parser (Maybe SyntaxKind)
peekKdMay = fmap (\(_, tok) -> tok.kindTL) <$> peekTokMay

startsTopDeclKd :: SyntaxKind -> Bool
startsTopDeclKd kd =
  case kd of
    DocLineTk -> True
    DocBlockTk -> True
    AtTk -> True
    LowerNameTk -> True
    TypeKwTk -> True
    EffectKwTk -> True
    ForeignKwTk -> True
    InfixKwTk -> True
    RegionKwTk -> True
    AnchorKwTk -> True
    CellKwTk -> True
    NativeKwTk -> True
    ThreadKwTk -> True
    _ -> False