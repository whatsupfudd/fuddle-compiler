{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.ModGraph.GraphTypes where

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)


import Fuddle.Compiler.Base.Core (Hash64)
import Fuddle.Compiler.ModGraph.Types (
        GraphId, NodeId, EdgeId, SccId, PkgId, RootId, ScopeModGraph
      , StatusNodeModGraph, StatusEdgeModGraph
      , RootKindMod
  )
import Fuddle.Compiler.ModGraph.Header (ImportHdrMod(..))
import Fuddle.Compiler.ModGraph.Origin (OriginMod(..), PkgRefMod(..), SourceLocMod(..))
import Fuddle.Compiler.ModGraph.Name (ModName(..))

-- # 14. Core graph data model

data KindDepModGraph
  = SourceDepModGraph
  | InterfaceDepModGraph
  | RuntimeDepModGraph
  deriving stock (Eq, Ord, Show)


data MetaGraphMod = MetaGraphMod
  { uid :: !GraphId
  , hashWorkspace :: !Hash64
  , version :: !Word64
  }
  deriving stock (Eq, Show)

data RootModGraph = RootModGraph
  { uid :: !RootId
  , name :: !Text
  , kind :: !RootKindMod
  , path :: !FilePath
  , pkgMay :: !(Maybe PkgRefMod)
  }
  deriving stock (Eq, Show)

data NodeModGraph = NodeModGraph
  { uid :: !NodeId
  , name :: !ModName
  , origin :: !OriginMod
  , scope :: !ScopeModGraph
  , status :: !StatusNodeModGraph

  , hashSource :: !Hash64
  , hashHeader :: !Hash64
  , hashIfaceMay :: !(Maybe Hash64)

  , imports :: !(Vector EdgeId)
  , importedBy :: !(Vector EdgeId)
  , scc :: !SccId
  }
  deriving stock (Eq, Show)

data EdgeModGraph = EdgeModGraph
  { uid :: !EdgeId
  , from :: !NodeId
  , importHdr :: !ImportHdrMod
  , toMay :: !(Maybe NodeId)
  , kind :: !KindDepModGraph
  , status :: !StatusEdgeModGraph
  }
  deriving stock (Eq, Show)

data SccModGraph = SccModGraph
  { uid :: !SccId
  , nodes :: !(Vector NodeId)
  , deps :: !(Vector SccId)
  , indegree :: !Int
  , topoIx :: !Int
  , cyclic :: !Bool
  }
  deriving stock (Eq, Show)

data ModGraph = ModGraph
  { meta :: !MetaGraphMod
  , roots :: !(Vector RootModGraph)
  , nodes :: !(IntMap NodeModGraph)
  , edges :: !(IntMap EdgeModGraph)
  , sccs :: !(IntMap SccModGraph)

  , byName :: !(Map ModName (Vector NodeId))
  , byOrigin :: !(Map SourceLocMod NodeId)
  }
  deriving stock (Eq, Show)