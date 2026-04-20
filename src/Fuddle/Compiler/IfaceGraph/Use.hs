{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.IfaceGraph.Use
  ( KindDepIfaceGraph(..)
  , UseIfaceGraph(..)
  , DepUseIfaceGraph(..)
  , UseSummaryIfaceGraph(..)
  , normUsesIfaceGraph
  , normDepUseIfaceGraph
  , normDepUsesIfaceGraph
  , normUseSummaryIfaceGraph
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import Fuddle.Compiler.ModGraph (NodeId)
import Fuddle.Compiler.IfaceGraph.Types (
    KindDepIfaceGraph(..), UseIfaceGraph(..), DepUseIfaceGraph(..), UseSummaryIfaceGraph(..)
  )

normUsesIfaceGraph :: Vector UseIfaceGraph -> Vector UseIfaceGraph
normUsesIfaceGraph uses0 =
  let
    uses1 =
      if hasAllUseIfaceGraph uses0
        then V.filter keepWithAllUseIfaceGraph uses0
        else uses0
    uses2 = Set.fromList (V.toList uses1)
  in
  V.fromList (Set.toAscList uses2)

normDepUseIfaceGraph :: DepUseIfaceGraph -> DepUseIfaceGraph
normDepUseIfaceGraph dep0 =
  DepUseIfaceGraph
    { to = dep0.to
    , kind = dep0.kind
    , uses = normUsesIfaceGraph dep0.uses
    }

normDepUsesIfaceGraph :: Vector DepUseIfaceGraph -> Vector DepUseIfaceGraph
normDepUsesIfaceGraph deps0 =
  let
    depMap =
      V.foldl'
        insDepUseIfaceGraph
        Map.empty
        deps0
  in
  V.fromList (map snd (Map.toAscList depMap))

normUseSummaryIfaceGraph :: UseSummaryIfaceGraph -> UseSummaryIfaceGraph
normUseSummaryIfaceGraph summary0 =
  UseSummaryIfaceGraph
    { from = summary0.from
    , deps = normDepUsesIfaceGraph summary0.deps
    }

insDepUseIfaceGraph
  :: Map.Map (NodeId, KindDepIfaceGraph) DepUseIfaceGraph
  -> DepUseIfaceGraph
  -> Map.Map (NodeId, KindDepIfaceGraph) DepUseIfaceGraph
insDepUseIfaceGraph depMap0 dep0 =
  let
    dep1 = normDepUseIfaceGraph dep0
    key1 = (dep1.to, dep1.kind)
  in
  Map.insertWith mergeDepUseIfaceGraph key1 dep1 depMap0

mergeDepUseIfaceGraph :: DepUseIfaceGraph -> DepUseIfaceGraph -> DepUseIfaceGraph
mergeDepUseIfaceGraph depNew depOld =
  DepUseIfaceGraph
    { to = depOld.to
    , kind = depOld.kind
    , uses = normUsesIfaceGraph (depOld.uses <> depNew.uses)
    }

hasAllUseIfaceGraph :: Vector UseIfaceGraph -> Bool
hasAllUseIfaceGraph =
  V.any isAllUseIfaceGraph

isAllUseIfaceGraph :: UseIfaceGraph -> Bool
isAllUseIfaceGraph use0 =
  case use0 of
    AllUseIfaceGraph -> True
    _ -> False

keepWithAllUseIfaceGraph :: UseIfaceGraph -> Bool
keepWithAllUseIfaceGraph use0 =
  case use0 of
    NamespaceUseIfaceGraph -> True
    AllUseIfaceGraph -> True
    _ -> False