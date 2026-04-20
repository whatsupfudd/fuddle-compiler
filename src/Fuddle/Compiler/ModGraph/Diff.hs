{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.ModGraph.Diff
  ( DiffModGraph(..)
  , diffModGraph
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V

import Fuddle.Compiler.ModGraph.Types ( EdgeId(..), NodeId(..), SccId(..), StatusEdgeModGraph )
import Fuddle.Compiler.ModGraph.GraphTypes (EdgeModGraph(..), ModGraph(..), NodeModGraph(..), SccModGraph(..), KindDepModGraph(..))

data DiffModGraph = DiffModGraph
  { addNodes :: !(Vector NodeId)
  , delNodes :: !(Vector NodeId)
  , addEdges :: !(Vector EdgeId)
  , delEdges :: !(Vector EdgeId)
  , changedSccs :: !(Vector SccId)
  , changedIfaces :: !(Vector NodeId)
  }
  deriving stock (Eq, Show)

diffModGraph :: ModGraph -> ModGraph -> DiffModGraph
diffModGraph prevGraph nextGraph =
  let
    prevNodes = indexNodesModGraph prevGraph
    nextNodes = indexNodesModGraph nextGraph
    prevEdges = indexEdgesModGraph prevGraph
    nextEdges = indexEdgesModGraph nextGraph
    prevSccs = indexSccsModGraph prevGraph
    nextSccs = indexSccsModGraph nextGraph

    addNodeSet = diffKeysMap nextNodes prevNodes
    delNodeSet = diffKeysMap prevNodes nextNodes
    addEdgeSet = diffKeysMap nextEdges prevEdges
    delEdgeSet = diffKeysMap prevEdges nextEdges

    changedIfaceSet = changedIfacesModGraph prevNodes nextNodes
    changedSccSet =
      changedSccsModGraph
        prevNodes
        nextNodes
        prevEdges
        nextEdges
        prevSccs
        nextSccs
        addNodeSet
        addEdgeSet
        delEdgeSet
  in
  DiffModGraph
    { addNodes = vectorSet addNodeSet
    , delNodes = vectorSet delNodeSet
    , addEdges = vectorSet addEdgeSet
    , delEdges = vectorSet delEdgeSet
    , changedSccs = vectorSet changedSccSet
    , changedIfaces = vectorSet changedIfaceSet
    }

indexNodesModGraph :: ModGraph -> Map NodeId NodeModGraph
indexNodesModGraph graph0 =
  IntMap.foldr' (\node0 acc -> Map.insert node0.uid node0 acc) Map.empty graph0.nodes

indexEdgesModGraph :: ModGraph -> Map EdgeId EdgeModGraph
indexEdgesModGraph graph0 =
  IntMap.foldr' (\edge0 acc -> Map.insert edge0.uid edge0 acc) Map.empty graph0.edges

indexSccsModGraph :: ModGraph -> Map SccId SccModGraph
indexSccsModGraph graph0 =
  IntMap.foldr' (\scc0 acc -> Map.insert scc0.uid scc0 acc) Map.empty graph0.sccs

diffKeysMap :: Ord k => Map k a -> Map k b -> Set k
diffKeysMap leftMap rightMap = Map.keysSet (Map.difference leftMap rightMap)

vectorSet :: Set a -> Vector a
vectorSet set0 = V.fromList (Set.toAscList set0)

changedIfacesModGraph :: Map NodeId NodeModGraph -> Map NodeId NodeModGraph -> Set NodeId
changedIfacesModGraph prevNodes nextNodes =
  Set.fromList
    [ nodeId0
    | (nodeId0, nextNode0) <- Map.toAscList nextNodes
    , Just prevNode0 <- [Map.lookup nodeId0 prevNodes]
    , prevNode0.hashIfaceMay /= nextNode0.hashIfaceMay
    ]

changedSccsModGraph
  :: Map NodeId NodeModGraph
  -> Map NodeId NodeModGraph
  -> Map EdgeId EdgeModGraph
  -> Map EdgeId EdgeModGraph
  -> Map SccId SccModGraph
  -> Map SccId SccModGraph
  -> Set NodeId
  -> Set EdgeId
  -> Set EdgeId
  -> Set SccId
changedSccsModGraph prevNodes nextNodes prevEdges nextEdges prevSccs nextSccs addNodeSet addEdgeSet delEdgeSet =
  Set.unions
    [ changedSccsStructModGraph prevSccs nextSccs
    , changedSccsNodeMoveModGraph prevNodes nextNodes
    , touchedSccsAddNodesModGraph nextNodes addNodeSet
    , touchedSccsEdgeSetModGraph nextNodes nextEdges addEdgeSet
    , touchedSccsDelEdgesModGraph nextNodes prevEdges delEdgeSet
    , touchedSccsChangedEdgesModGraph prevEdges nextEdges nextNodes
    ]

changedSccsStructModGraph :: Map SccId SccModGraph -> Map SccId SccModGraph -> Set SccId
changedSccsStructModGraph prevSccs nextSccs =
  Set.fromList
    [ sccId0
    | (sccId0, nextScc0) <- Map.toAscList nextSccs
    , case Map.lookup sccId0 prevSccs of
        Nothing -> True
        Just prevScc0 -> shapeSccModGraph prevScc0 /= shapeSccModGraph nextScc0
    ]

changedSccsNodeMoveModGraph :: Map NodeId NodeModGraph -> Map NodeId NodeModGraph -> Set SccId
changedSccsNodeMoveModGraph prevNodes nextNodes =
  Set.fromList
    [ nextNode0.scc
    | (nodeId0, nextNode0) <- Map.toAscList nextNodes
    , Just prevNode0 <- [Map.lookup nodeId0 prevNodes]
    , prevNode0.scc /= nextNode0.scc
    ]

touchedSccsAddNodesModGraph :: Map NodeId NodeModGraph -> Set NodeId -> Set SccId
touchedSccsAddNodesModGraph nextNodes addNodeSet =
  Set.fromList
    [ node0.scc
    | nodeId0 <- Set.toAscList addNodeSet
    , Just node0 <- [Map.lookup nodeId0 nextNodes]
    ]

touchedSccsEdgeSetModGraph :: Map NodeId NodeModGraph -> Map EdgeId EdgeModGraph -> Set EdgeId -> Set SccId
touchedSccsEdgeSetModGraph nextNodes edgeMap edgeIdSet =
  touchedSccsEdgesModGraph nextNodes $
    [ edge0
    | edgeId0 <- Set.toAscList edgeIdSet
    , Just edge0 <- [Map.lookup edgeId0 edgeMap]
    ]

touchedSccsDelEdgesModGraph :: Map NodeId NodeModGraph -> Map EdgeId EdgeModGraph -> Set EdgeId -> Set SccId
touchedSccsDelEdgesModGraph nextNodes prevEdges delEdgeSet =
  touchedSccsEdgesModGraph nextNodes $
    [ edge0
    | edgeId0 <- Set.toAscList delEdgeSet
    , Just edge0 <- [Map.lookup edgeId0 prevEdges]
    ]

touchedSccsChangedEdgesModGraph
  :: Map EdgeId EdgeModGraph
  -> Map EdgeId EdgeModGraph
  -> Map NodeId NodeModGraph
  -> Set SccId
touchedSccsChangedEdgesModGraph prevEdges nextEdges nextNodes =
  touchedSccsEdgesModGraph nextNodes $
    [ nextEdge0
    | (edgeId0, nextEdge0) <- Map.toAscList nextEdges
    , Just prevEdge0 <- [Map.lookup edgeId0 prevEdges]
    , shapeEdgeModGraph prevEdge0 /= shapeEdgeModGraph nextEdge0
    ]

touchedSccsEdgesModGraph :: Map NodeId NodeModGraph -> [EdgeModGraph] -> Set SccId
touchedSccsEdgesModGraph nextNodes edges0 =
  Set.fromList $
    [ node0.scc
    | edge0 <- edges0
    , nodeId0 <- edge0.from : maybeToList edge0.toMay
    , Just node0 <- [Map.lookup nodeId0 nextNodes]
    ]

shapeSccModGraph :: SccModGraph -> (Vector NodeId, Vector SccId, Int, Int, Bool)
shapeSccModGraph scc0 =
  (scc0.nodes, scc0.deps, scc0.indegree, scc0.topoIx, scc0.cyclic)

shapeEdgeModGraph :: EdgeModGraph -> (NodeId, Maybe NodeId, KindDepModGraph, StatusEdgeModGraph)
shapeEdgeModGraph edge0 =
  (edge0.from, edge0.toMay, edge0.kind, edge0.status)