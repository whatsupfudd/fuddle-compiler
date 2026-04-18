module Fuddle.Compiler.Parse.Internal.Foreign
  ( foreignBlockP
  , foreignExprP
  ) where

import Fuddle.Compiler.Parse.Internal.Combinator (emitTok, expectTok, matchTok, withNode)
import Fuddle.Compiler.Parse.Internal.State (Parser)
import Fuddle.Compiler.Syntax.Kind (SyntaxKind(..))

foreignBlockP :: Parser ()
foreignBlockP =
  withNode BlockForeignNd $ do
    expectTok ForeignKwTk
    delimForeignP LBraceTk RBraceTk

foreignExprP :: Parser ()
foreignExprP =
  withNode ExprForeignNd $ do
    expectTok ForeignKwTk
    delimForeignP LParenTk RParenTk

delimForeignP :: SyntaxKind -> SyntaxKind -> Parser ()
delimForeignP openKd closeKd = do
  expectTok openKd
  bodyForeignP closeKd
  expectTok closeKd
  -- TODO: should it return the bool?
  pure ()

bodyForeignP :: SyntaxKind -> Parser ()
bodyForeignP closeKd = do
  stop <- stopForeignP closeKd
  if stop
    then pure ()
    else do
      itemForeignP closeKd
      bodyForeignP closeKd

stopForeignP :: SyntaxKind -> Parser Bool
stopForeignP closeKd = do
  closeHit <- matchTok closeKd
  if closeHit
    then pure True
    else do
      endHit <- matchTok EndTk
      if endHit
        then pure True
        else do
          sepHit <- matchTok LayoutSepTk
          if sepHit
            then pure True
            else matchTok LayoutCloseTk

itemForeignP :: SyntaxKind -> Parser ()
itemForeignP closeKd = do
  parenHit <- matchTok LParenTk
  if parenHit
    then groupForeignP LParenTk RParenTk
    else do
      bracketHit <- matchTok LBracketTk
      if bracketHit
        then groupForeignP LBracketTk RBracketTk
        else do
          braceHit <- matchTok LBraceTk
          if braceHit
            then groupForeignP LBraceTk RBraceTk
            else do
              _ <- closeKd `seq` pure ()
              emitTok
              -- TODO: Should it return the bool?
              pure ()

groupForeignP :: SyntaxKind -> SyntaxKind -> Parser ()
groupForeignP openKd closeKd = do
  expectTok openKd
  bodyForeignP closeKd
  expectTok closeKd
  -- TODO: Should it return the bool?
  pure ()