{-# LANGUAGE AllowAmbiguousTypes #-}
module Fuddle.Compiler.Syntax.AstClass
  ( AstNode(..)
  , AstToken(..)
  ) where

import Fuddle.Compiler.Syntax.Kind (SyntaxKind (..))
import Fuddle.Compiler.Syntax.Red (NodeSyntax, TokenSyntax)

class AstNode a where
  canCastNode :: SyntaxKind -> Bool
  castNode :: NodeSyntax -> Maybe a
  nodeAst :: a -> NodeSyntax
  {-# MINIMAL canCastNode, castNode, nodeAst #-}

class AstToken a where
  canCastToken :: SyntaxKind -> Bool
  castToken :: TokenSyntax -> Maybe a
  tokenAst :: a -> TokenSyntax
  {-# MINIMAL canCastToken, castToken, tokenAst #-}

instance AstNode NodeSyntax where
  canCastNode _ = True
  castNode = Just
  nodeAst = id

instance AstToken TokenSyntax where
  canCastToken _ = True
  castToken = Just
  tokenAst = id