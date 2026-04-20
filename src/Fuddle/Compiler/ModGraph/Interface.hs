module Fuddle.Compiler.ModGraph.Interface
  ( IfaceMod(..)
  , DepIfaceMod(..)
  , attachIfaceHashMod
  , depsIfaceMod
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import Fuddle.Compiler.Base.Core (Hash64)
import Fuddle.Compiler.ModGraph
  ( EdgeId(..), ModGraph(..), NodeId(..) )
import Fuddle.Compiler.ModGraph.GraphTypes (
    EdgeModGraph(..), NodeModGraph(..), KindDepModGraph(..), ModGraph(..)
  )
import Fuddle.Compiler.ModGraph.Types (StatusEdgeModGraph(..))

data DepIfaceMod = DepIfaceMod
  { target :: !NodeId
  , hashIface :: !Hash64
  }
  deriving (Eq, Show)

data IfaceMod = IfaceMod
  { node :: !NodeId
  , hashIface :: !Hash64
  , deps :: !(Vector DepIfaceMod)
  }
  deriving (Eq, Show)

attachIfaceHashMod :: IfaceMod -> ModGraph -> ModGraph
attachIfaceHashMod iface graph =
  let
    nodeId0 = iface.node
    node0 = lookupNodeReqMod "attachIfaceHashMod" graph nodeId0
    node1 = node0 { hashIfaceMay = Just iface.hashIface }
    nodes1 = IntMap.insert (keyNodeMod "attachIfaceHashMod" nodeId0) node1 graph.nodes
  in
  graph { nodes = nodes1 }

depsIfaceMod :: ModGraph -> NodeId -> Vector DepIfaceMod
depsIfaceMod graph nodeId0 =
  let
    node0 = lookupNodeReqMod "depsIfaceMod" graph nodeId0
    depMap0 = Vector.foldl' (collectDepMapMod graph) Map.empty node0.imports
  in
  Vector.fromList
    [ DepIfaceMod { target = target0, hashIface = hash0 }
    | (target0, hash0) <- Map.toAscList depMap0
    ]

collectDepMapMod :: ModGraph -> Map.Map NodeId Hash64 -> EdgeId -> Map.Map NodeId Hash64
collectDepMapMod graph depMap edgeId0 =
  let
    edge0 = lookupEdgeReqMod "depsIfaceMod" graph edgeId0
  in
  if not (trackIfaceEdgeMod edge0)
    then depMap
    else case edge0.toMay of
      Nothing ->
        error
          (errInterfaceMod "depsIfaceMod"
            ("ready dependency edge has no target: " <> show edgeId0))
      Just target0 ->
        let
          node0 = lookupNodeReqMod "depsIfaceMod" graph target0
        in
        case node0.hashIfaceMay of
          Nothing -> depMap
          Just hash0 -> insertDepMapReqMod "depsIfaceMod" target0 hash0 depMap

trackIfaceEdgeMod :: EdgeModGraph -> Bool
trackIfaceEdgeMod edge =
  edge.status == ReadyEdgeStatusModGraph
    && case edge.kind of
      SourceDepModGraph -> True
      InterfaceDepModGraph -> True
      RuntimeDepModGraph -> False

insertDepMapReqMod :: String -> NodeId -> Hash64 -> Map.Map NodeId Hash64 -> Map.Map NodeId Hash64
insertDepMapReqMod fun target0 hash0 depMap =
  case Map.lookup target0 depMap of
    Nothing -> Map.insert target0 hash0 depMap
    Just hash1
      | hash1 == hash0 -> depMap
      | otherwise ->
          error
            (errInterfaceMod fun
              ( "conflicting interface hashes for target "
                  <> show target0
                  <> ": "
                  <> show hash1
                  <> " vs "
                  <> show hash0
              ))

lookupNodeReqMod :: String -> ModGraph -> NodeId -> NodeModGraph
lookupNodeReqMod fun graph nodeId0 =
  case IntMap.lookup (keyNodeMod fun nodeId0) graph.nodes of
    Just node0 -> node0
    Nothing -> error (errInterfaceMod fun ("missing node: " <> show nodeId0))

lookupEdgeReqMod :: String -> ModGraph -> EdgeId -> EdgeModGraph
lookupEdgeReqMod fun graph edgeId0 =
  case IntMap.lookup (keyEdgeMod fun edgeId0) graph.edges of
    Just edge0 -> edge0
    Nothing -> error (errInterfaceMod fun ("missing edge: " <> show edgeId0))

keyNodeMod :: String -> NodeId -> Int
keyNodeMod fun (NodeId n) = keyWord64ReqMod fun "NodeId" n

keyEdgeMod :: String -> EdgeId -> Int
keyEdgeMod fun (EdgeId n) = keyWord64ReqMod fun "EdgeId" n

keyWord64ReqMod :: String -> String -> Word64 -> Int
keyWord64ReqMod fun label n
  | n > fromIntegral (maxBound :: Int) =
      error
        (errInterfaceMod fun
          (label <> " exceeds Int range: " <> show n))
  | otherwise = fromIntegral n

errInterfaceMod :: String -> String -> String
errInterfaceMod fun msg = "Fuddle.Compiler.ModGraph.Interface." <> fun <> ": " <> msg