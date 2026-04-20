{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.ModGraph.Build where

import Data.Vector (Vector)
import Fuddle.Compiler.ModGraph.Types (NodeId, RootId)
import Fuddle.Compiler.ModGraph.Origin (SourceLocMod(..))
import Fuddle.Compiler.ModGraph.GraphTypes (RootModGraph, ModGraph)
import Fuddle.Compiler.Base.Diag (Diag)

data BuildReqModGraph = BuildReqModGraph
  { roots :: !(Vector RootModGraph)
  , includeDeps :: !Bool
  , prevMay :: !(Maybe ModGraph)
  }

data BuildResModGraph = BuildResModGraph
  { graph :: !ModGraph
  , diags :: !(Vector Diag)
  }

data KindChangeModGraph
  = AddChangeModGraph
  | RemoveChangeModGraph
  | SourceChangeModGraph
  | HeaderChangeModGraph
  | IfaceChangeModGraph
  | RootChangeModGraph
  deriving stock (Eq, Ord, Show)

data ChangeModGraph = ChangeModGraph
  { kind :: !KindChangeModGraph
  , nodeMay :: !(Maybe NodeId)
  , originMay :: !(Maybe SourceLocMod)
  }
  deriving stock (Eq, Show)

data UpdateReqModGraph = UpdateReqModGraph
  { prev :: !ModGraph
  , changedRoots :: !(Vector RootId)
  , changedSources :: !(Vector SourceLocMod)
  , changedIfaces :: !(Vector NodeId)
  }

data UpdateResModGraph = UpdateResModGraph
  { graph :: !ModGraph
  , changes :: !(Vector ChangeModGraph)
  , diags :: !(Vector Diag)
  }