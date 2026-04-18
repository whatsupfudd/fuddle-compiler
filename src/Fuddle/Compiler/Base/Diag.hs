{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Base.Diag
  ( StageDiag(..)
  , SeverityDiag(..)
  , CodeDiag(..)
  , RelatedDiag(..)
  , FixDiag(..)
  , Diag(..)
  , mkDiag
  , addRelatedDiag
  , addFixDiag
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fuddle.Compiler.Base.Range (Range)

data StageDiag =
    LexDG
  | LayoutDG
  | ParseDG
  | BuildDG
  | RedDG
  deriving stock (Eq, Ord, Show)

data SeverityDiag =
    ErrorDS
  | WarningDS
  | InfoDS
  | HintDS
  deriving stock (Eq, Ord, Show)

newtype CodeDiag = CodeDiag Text
  deriving stock (Eq, Ord, Show)

data RelatedDiag = RelatedDiag {
    rangeRD :: !Range
  , msgRD :: !Text
  }
  deriving stock (Eq, Show)

data FixDiag = FixDiag {
    titleFD :: !Text
  , replaceFD :: !Range
  , withFD :: !Text
  }
  deriving stock (Eq, Show)

data Diag = Diag {
    codeDG :: !CodeDiag
  , stageDG :: !StageDiag
  , severityDG :: !SeverityDiag
  , rangeDG :: !Range
  , msgDG :: !Text
  , relatedDG :: !(Vector RelatedDiag)
  , fixesDG :: !(Vector FixDiag)
  , notesDG :: !(Vector Text)
  }
  deriving stock (Eq, Show)

mkDiag :: CodeDiag -> StageDiag -> SeverityDiag -> Range -> Text -> Diag
mkDiag code stage severity range msg = Diag { 
      codeDG = code
    , stageDG = stage
    , severityDG = severity
    , rangeDG = range
    , msgDG = msg
    , relatedDG = V.empty
    , fixesDG = V.empty
    , notesDG = V.empty
    }

addRelatedDiag :: RelatedDiag -> Diag -> Diag
addRelatedDiag related diag = diag { relatedDG = V.snoc diag.relatedDG related }

addFixDiag :: FixDiag -> Diag -> Diag
addFixDiag fix diag = diag { fixesDG = V.snoc diag.fixesDG fix }