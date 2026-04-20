{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.IfaceGraph.Invalidate
  ( FrontierIfaceGraph(..)
  , ReasonInvalidateIfaceGraph(..)
  , MarkInvalidateIfaceGraph(..)
  , PlanInvalidateIfaceGraph(..)
  ) where

import Data.Vector (Vector)
import Fuddle.Compiler.IfaceGraph.Types (IfaceEdgeId, StageFrontIfaceGraph)
import Fuddle.Compiler.IfaceGraph.Use (UseIfaceGraph)
import Fuddle.Compiler.ModGraph (NodeId, SccId)

data FrontierIfaceGraph = FrontierIfaceGraph
  { body :: !(Vector NodeId)
  , iface :: !(Vector NodeId)
  , lower :: !(Vector NodeId)
  , emit :: !(Vector NodeId)
  }
  deriving stock (Eq, Show)

data ReasonInvalidateIfaceGraph
  = ApiReasonInvalidateIfaceGraph !NodeId !IfaceEdgeId !(Vector UseIfaceGraph)
  | ReExportReasonInvalidateIfaceGraph !NodeId !IfaceEdgeId !(Vector UseIfaceGraph)
  | InlineReasonInvalidateIfaceGraph !NodeId !IfaceEdgeId
  | SccReasonInvalidateIfaceGraph !SccId
  deriving stock (Eq, Show)

data MarkInvalidateIfaceGraph = MarkInvalidateIfaceGraph
  { stage :: !StageFrontIfaceGraph
  , node :: !NodeId
  , reason :: !ReasonInvalidateIfaceGraph
  }
  deriving stock (Eq, Show)

data PlanInvalidateIfaceGraph = PlanInvalidateIfaceGraph
  { frontier :: !FrontierIfaceGraph
  , marks :: !(Vector MarkInvalidateIfaceGraph)
  }
  deriving stock (Eq, Show)
