{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuddle.Compiler.Syntax.Token
  ( TextKey(..)
  , BlobKey(..)
  , TokIx(..)
  , LexemeRef(..)
  , TokenOrigin(..)
  , SyntheticReason(..)
  , TokenFlags(..)
  , triviaTF
  , docTF
  , syntheticTF
  , missingTF
  , hasTokenFlag
  , TokenLex(..)
  , TokenStream
  , fromVectorToks
  , toVectorToks
  , countToks
  , lookupTok
  ) where

import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16)
import Fuddle.Compiler.Base.Range (Range)
import Fuddle.Compiler.Syntax.Kind (SyntaxKind)

newtype TextKey = TextKey Int32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype BlobKey = BlobKey Int32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype TokIx = TokIx Int32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

data LexemeRef
  = ImplicitLR
  | InternLR !TextKey
  | BlobLR !BlobKey
  deriving stock (Eq, Ord, Show)

data TokenOrigin
  = OriginalTO
  | SyntheticTO !SyntheticReason
  deriving stock (Eq, Ord, Show)

data SyntheticReason
  = LayoutSR
  | RecoverySR
  | GeneratedSR
  deriving stock (Eq, Ord, Show)

newtype TokenFlags = TokenFlags Word16
  deriving stock (Eq, Ord, Show)


data TokenLex = TokenLex
  { kindTL :: !SyntaxKind
  , rangeTL :: !Range
  , lexemeRefTL :: !LexemeRef
  , originTL :: !TokenOrigin
  , flagsTL :: !TokenFlags
  }
  deriving stock (Eq, Show)

newtype TokenStream = TokenStream
  { toksTS :: Vector TokenLex
  }
  deriving stock (Eq, Show)


triviaTF :: TokenFlags
triviaTF = TokenFlags 0x0001

docTF :: TokenFlags
docTF = TokenFlags 0x0002

syntheticTF :: TokenFlags
syntheticTF = TokenFlags 0x0004

missingTF :: TokenFlags
missingTF = TokenFlags 0x0008

hasTokenFlag :: TokenFlags -> TokenFlags -> Bool
hasTokenFlag (TokenFlags xs) (TokenFlags ys) = (xs .&. ys) == ys

fromVectorToks :: Vector TokenLex -> TokenStream
fromVectorToks toks0 = TokenStream { toksTS = toks0 }

toVectorToks :: TokenStream -> Vector TokenLex
toVectorToks stream = stream.toksTS

countToks :: TokenStream -> Int
countToks stream = V.length stream.toksTS

lookupTok :: TokenStream -> TokIx -> Maybe TokenLex
lookupTok stream (TokIx ix0)
  | ix0 < 0 = Nothing
  | otherwise = stream.toksTS V.!? fromIntegral ix0