{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.IfaceGraph.Export
  ( ExportKeyIfaceGraph(..)
  , ExportIfaceGraph(..)
  ) where

import Data.Text (Text)
import Fuddle.Compiler.Base.Core (Hash64)

data ExportKeyIfaceGraph
  = ValueKeyIfaceGraph !Text
  | TypeKeyIfaceGraph !Text
  | CtorKeyIfaceGraph !Text
  | AliasKeyIfaceGraph !Text
  | EffectKeyIfaceGraph !Text
  | ForeignKeyIfaceGraph !Text
  deriving stock (Eq, Ord, Show)

data ExportIfaceGraph
  = ValueExportIfaceGraph !Text !Hash64
  | TypeExportIfaceGraph !Text !Hash64
  | CtorExportIfaceGraph !Text !Hash64
  | AliasExportIfaceGraph !Text !Hash64
  | EffectExportIfaceGraph !Text !Hash64
  | ForeignExportIfaceGraph !Text !Hash64
  deriving stock (Eq, Show)