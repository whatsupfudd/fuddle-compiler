{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuddle.Compiler.IfaceGraph.Types where

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Data.Vector (Vector)

import Fuddle.Compiler.Base.Core (Hash64)
import Fuddle.Compiler.ModGraph ( NodeId(..), SccId(..), ModGraph )
import Fuddle.Compiler.IfaceGraph.Export (ExportIfaceGraph)
import Fuddle.Compiler.Base.Diag (Diag)


newtype IfaceGraphId = IfaceGraphId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype IfaceEdgeId = IfaceEdgeId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

data MetaIfaceGraph = MetaIfaceGraph
  { uid :: !IfaceGraphId
  , hashModGraph :: !Hash64
  , version :: !Word64
  }
  deriving stock (Eq, Show)

data UnitIfaceGraph = UnitIfaceGraph
  { node :: !NodeId
  , scc :: !SccId
  , status :: !StatusNodeIfaceGraph

  , hashApi :: !Hash64
  , hashNamespace :: !Hash64
  , hashInline :: !Hash64

  , exports :: !(Vector ExportIfaceGraph)
  }
  deriving stock (Eq, Show)

data EdgeIfaceGraph = EdgeIfaceGraph
  { uid :: !IfaceEdgeId
  , from :: !NodeId
  , to :: !NodeId
  , kind :: !KindDepIfaceGraph
  , status :: !StatusEdgeIfaceGraph
  , uses :: !(Vector UseIfaceGraph)

  , seenApi :: !Hash64
  , seenInline :: !Hash64
  }
  deriving stock (Eq, Show)

data IfaceGraph = IfaceGraph
  { meta :: !MetaIfaceGraph
  , modGraph :: !ModGraph
  , units :: !(IntMap UnitIfaceGraph)
  , edges :: !(IntMap EdgeIfaceGraph)
  , byFrom :: !(Map NodeId (Vector IfaceEdgeId))
  , byTo :: !(Map NodeId (Vector IfaceEdgeId))
  }
  deriving stock (Eq, Show)
  
-- From the .Use module:

data KindDepIfaceGraph
  = ApiDepIfaceGraph
  | ReExportDepIfaceGraph
  | InlineDepIfaceGraph
  deriving stock (Eq, Ord, Show)

data UseIfaceGraph
  = NamespaceUseIfaceGraph
  | AllUseIfaceGraph
  | ValueUseIfaceGraph !Text
  | TypeUseIfaceGraph !Text
  | CtorUseIfaceGraph !Text
  | AliasUseIfaceGraph !Text
  | EffectUseIfaceGraph !Text
  | ForeignUseIfaceGraph !Text
  deriving stock (Eq, Ord, Show)

data DepUseIfaceGraph = DepUseIfaceGraph
  { to :: !NodeId
  , kind :: !KindDepIfaceGraph
  , uses :: !(Vector UseIfaceGraph)
  }
  deriving stock (Eq, Show)

data UseSummaryIfaceGraph = UseSummaryIfaceGraph
  { from :: !NodeId
  , deps :: !(Vector DepUseIfaceGraph)
  }
  deriving stock (Eq, Show)

-- For .Validate module:


data StatusNodeIfaceGraph
  = ReadyNodeStatusIfaceGraph
  | FailedNodeStatusIfaceGraph
  | StaleNodeStatusIfaceGraph
  deriving stock (Eq, Ord, Show)

data StatusEdgeIfaceGraph
  = ReadyEdgeStatusIfaceGraph
  | StaleEdgeStatusIfaceGraph
  | BlockedEdgeStatusIfaceGraph
  deriving stock (Eq, Ord, Show)


data StageFrontIfaceGraph
  = BodyStageFrontIfaceGraph
  | IfaceStageFrontIfaceGraph
  | LowerStageFrontIfaceGraph
  | EmitStageFrontIfaceGraph
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- # 12. Build/update pipeline records
data BuildResIfaceGraph = BuildResIfaceGraph
  { graph :: !IfaceGraph
  , diags :: !(Vector Diag)
  }
  deriving stock (Eq, Show)

data UpdateReqIfaceGraph = UpdateReqIfaceGraph
  { prev :: !IfaceGraph
  , modGraph :: !ModGraph
  , unitsUpd :: !(Vector UnitIfaceGraph)
  , usesUpd :: !(Vector UseSummaryIfaceGraph)
  }
  deriving stock (Eq, Show)
