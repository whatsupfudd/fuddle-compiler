{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fuddle.Compiler.ModGraph.Types where

import Data.Word (Word64)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Fuddle.Compiler.Base.Core (Hash64)

newtype GraphId = GraphId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype NodeId = NodeId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype EdgeId = EdgeId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype SccId = SccId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype PkgId = PkgId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype RootId = RootId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

-- Graph node and edge statuses:
data StatusNodeModGraph
  = ReadyNodeStatusModGraph
  | HeaderErrNodeStatusModGraph
  | MissingNodeStatusModGraph
  | ShadowedNodeStatusModGraph
  deriving stock (Eq, Ord, Show)

data StatusEdgeModGraph
  = ReadyEdgeStatusModGraph
  | MissingEdgeStatusModGraph
  | AmbiguousEdgeStatusModGraph
  | HiddenEdgeStatusModGraph
  | SelfEdgeStatusModGraph
  deriving stock (Eq, Ord, Show)

-- Resolution:
data ScopeModGraph
  = WorkspaceScopeModGraph
  | DependencyScopeModGraph
  | HiddenScopeModGraph
  deriving stock (Eq, Ord, Show)

-- Origin:
data RootKindMod
  = WorkspaceRootMod
  | PackageRootMod
  | RegistryRootMod
  | VirtualRootMod
  deriving stock (Eq, Ord, Show)

