{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.ModGraph.Origin
  ( OriginMod(..)
  , SourceLocMod(..)
  , RootKindMod(..)
  , PkgRefMod(..)
  ) where

import Data.Text (Text)
import Fuddle.Compiler.ModGraph.Types (PkgId, RootId, RootKindMod(..))

data PkgRefMod = PkgRefMod
  { uid :: !PkgId
  , name :: !Text
  , version :: !Text
  }
  deriving stock (Eq, Show)

data SourceLocMod
  = FileSourceLocMod !FilePath
  | ArchiveSourceLocMod !Text !FilePath
  | VirtualSourceLocMod !Text
  deriving stock (Eq, Ord, Show)

data OriginMod = OriginMod
  { root :: !RootId
  , rootKind :: !RootKindMod
  , pkg :: !PkgRefMod
  , loc :: !SourceLocMod
  }
  deriving stock (Eq, Show)