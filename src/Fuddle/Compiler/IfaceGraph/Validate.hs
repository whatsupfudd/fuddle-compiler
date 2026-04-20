{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.IfaceGraph.Validate
  ( validateIfaceGraph
  ) where

import Data.List (find)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)


import Fuddle.Compiler.Base.Core (Hash64)
import Fuddle.Compiler.Base.Diag (Diag, mkDiag, CodeDiag (..), StageDiag (..), SeverityDiag (..))

import Fuddle.Compiler.ModGraph ( NodeId(..), StatusEdgeModGraph(..), ModGraph )

import Fuddle.Compiler.IfaceGraph.Export (ExportIfaceGraph(..), ExportKeyIfaceGraph(..))
import Fuddle.Compiler.IfaceGraph.Types ( 
    IfaceEdgeId(..), StatusEdgeIfaceGraph(..), StatusNodeIfaceGraph(..)
    , IfaceGraph(..), UnitIfaceGraph(..), EdgeIfaceGraph(..)
    , UseIfaceGraph(..)
  )
import Fuddle.Compiler.ModGraph.GraphTypes
import Fuddle.Compiler.Base.Range (emptyRange)

-- , IfaceGraph(..), UnitIfaceGraph(..),  EdgeIfaceGraph(..)
-- Fuddle.Compiler.ModGraph: EdgeModGraph(..)  , ModGraph(..), NodeModGraph(..)


validateIfaceGraph :: IfaceGraph -> Vector Diag
validateIfaceGraph graph0 =
  V.fromList $
    concat
      [ validateUnitsIfaceGraph graph0
      , validateEdgesIfaceGraph graph0
      , validateByFromIfaceGraph graph0
      , validateByToIfaceGraph graph0
      ]

validateUnitsIfaceGraph :: IfaceGraph -> [Diag]
validateUnitsIfaceGraph graph0 =
  validateDupUnitsIfaceGraph graph0
    <> concatMap (validateUnitEntryIfaceGraph graph0) (IntMap.toList graph0.units)

validateEdgesIfaceGraph :: IfaceGraph -> [Diag]
validateEdgesIfaceGraph graph0 =
  validateDupEdgesIfaceGraph graph0
    <> concatMap (validateEdgeEntryIfaceGraph graph0) (IntMap.toList graph0.edges)

validateByFromIfaceGraph :: IfaceGraph -> [Diag]
validateByFromIfaceGraph graph0 =
  concatMap (validateByFromEntryIfaceGraph graph0) (Map.toList graph0.byFrom)
    <> concatMap (validateByFromCoverageIfaceGraph graph0) (IntMap.elems graph0.edges)

validateByToIfaceGraph :: IfaceGraph -> [Diag]
validateByToIfaceGraph graph0 =
  concatMap (validateByToEntryIfaceGraph graph0) (Map.toList graph0.byTo)
    <> concatMap (validateByToCoverageIfaceGraph graph0) (IntMap.elems graph0.edges)

validateDupUnitsIfaceGraph :: IfaceGraph -> [Diag]
validateDupUnitsIfaceGraph graph0 =
  let
    dupNodes = dupValsValidateIfaceGraph [unit0.node | unit0 <- IntMap.elems graph0.units]
  in
  [ diagValidateIfaceGraph "IFG-VAL-UNIT-DUP"
      ("duplicate interface units for node " <> showTextValidateIfaceGraph node0)
  | node0 <- dupNodes
  ]

validateUnitEntryIfaceGraph :: IfaceGraph -> (Int, UnitIfaceGraph) -> [Diag]
validateUnitEntryIfaceGraph graph0 (key0, unit0) =
  let
    nodeMay = lookupNodeLooseValidateIfaceGraph graph0.modGraph unit0.node
    keyDiag =
      [ diagValidateIfaceGraph "IFG-VAL-UNIT-KEY"
          ("unit table key " <> showTextValidateIfaceGraph key0
            <> " does not match stored node " <> showTextValidateIfaceGraph unit0.node)
      | key0 /= keyNodeValidateIfaceGraph unit0.node
      ]
    nodeDiag =
      [ diagValidateIfaceGraph "IFG-VAL-UNIT-NODE"
          ("interface unit refers to missing module-graph node "
            <> showTextValidateIfaceGraph unit0.node)
      | nodeMay == Nothing
      ]
    sccDiag =
      case nodeMay of
        Just node0
          | unit0.scc /= node0.scc ->
              [ diagValidateIfaceGraph "IFG-VAL-UNIT-SCC"
                  ("interface unit SCC " <> showTextValidateIfaceGraph unit0.scc
                    <> " does not match module-graph SCC " <> showTextValidateIfaceGraph node0.scc
                    <> " for node " <> showTextValidateIfaceGraph unit0.node)
              ]
        _ -> []
    exportDiag = validateExportsUnitIfaceGraph unit0
  in
  keyDiag <> nodeDiag <> sccDiag <> exportDiag

validateExportsUnitIfaceGraph :: UnitIfaceGraph -> [Diag]
validateExportsUnitIfaceGraph unit0 =
  let
    dupKeys = dupValsValidateIfaceGraph (map keyExportValidateIfaceGraph (V.toList unit0.exports))
  in
  [ diagValidateIfaceGraph "IFG-VAL-UNIT-EXPORT-DUP"
      ("duplicate export key " <> showTextValidateIfaceGraph key0
        <> " in interface unit for node " <> showTextValidateIfaceGraph unit0.node)
  | key0 <- dupKeys
  ]

validateDupEdgesIfaceGraph :: IfaceGraph -> [Diag]
validateDupEdgesIfaceGraph graph0 =
  let
    dupEdges = dupValsValidateIfaceGraph [edge0.uid | edge0 <- IntMap.elems graph0.edges]
  in
  [ diagValidateIfaceGraph "IFG-VAL-EDGE-DUP"
      ("duplicate interface edge id " <> showTextValidateIfaceGraph edgeId0)
  | edgeId0 <- dupEdges
  ]

validateEdgeEntryIfaceGraph :: IfaceGraph -> (Int, EdgeIfaceGraph) -> [Diag]
validateEdgeEntryIfaceGraph graph0 (key0, edge0) =
  let
    srcNodeMay = lookupNodeLooseValidateIfaceGraph graph0.modGraph edge0.from
    tgtNodeMay = lookupNodeLooseValidateIfaceGraph graph0.modGraph edge0.to
    srcUnitMay = lookupUnitLooseValidateIfaceGraph graph0 edge0.from
    tgtUnitMay = lookupUnitLooseValidateIfaceGraph graph0 edge0.to
    keyDiag =
      [ diagValidateIfaceGraph "IFG-VAL-EDGE-KEY"
          ("edge table key " <> showTextValidateIfaceGraph key0
            <> " does not match stored edge id " <> showTextValidateIfaceGraph edge0.uid)
      | key0 /= keyIfaceEdgeValidateIfaceGraph edge0.uid
      ]
    srcDiag =
      [ diagValidateIfaceGraph "IFG-VAL-EDGE-SRC"
          ("interface edge " <> showTextValidateIfaceGraph edge0.uid
            <> " refers to missing source node " <> showTextValidateIfaceGraph edge0.from)
      | srcNodeMay == Nothing
      ]
    tgtDiag =
      [ diagValidateIfaceGraph "IFG-VAL-EDGE-TGT"
          ("interface edge " <> showTextValidateIfaceGraph edge0.uid
            <> " refers to missing target node " <> showTextValidateIfaceGraph edge0.to)
      | tgtNodeMay == Nothing
      ]
    depDiag =
      [ diagValidateIfaceGraph "IFG-VAL-EDGE-DEP"
          ("interface edge " <> showTextValidateIfaceGraph edge0.uid
            <> " from " <> showTextValidateIfaceGraph edge0.from
            <> " to " <> showTextValidateIfaceGraph edge0.to
            <> " has no corresponding ready module-graph dependency")
      | srcNodeMay /= Nothing
      , tgtNodeMay /= Nothing
      , not (hasDepValidateIfaceGraph graph0.modGraph edge0.from edge0.to)
      ]
    usesDiag =
      [ diagValidateIfaceGraph "IFG-VAL-EDGE-USES"
          ("uses vector is not normalized on edge " <> showTextValidateIfaceGraph edge0.uid
            <> "; got " <> showTextValidateIfaceGraph edge0.uses
            <> ", expected " <> showTextValidateIfaceGraph (normUsesValidateIfaceGraph edge0.uses))
      | edge0.uses /= normUsesValidateIfaceGraph edge0.uses
      ]
    seenDiag = validateSeenEdgeIfaceGraph edge0 tgtUnitMay
    statusDiag = validateStatusEdgeIfaceGraph edge0 srcUnitMay tgtUnitMay
  in
  keyDiag <> srcDiag <> tgtDiag <> depDiag <> usesDiag <> seenDiag <> statusDiag

validateSeenEdgeIfaceGraph :: EdgeIfaceGraph -> Maybe UnitIfaceGraph -> [Diag]
validateSeenEdgeIfaceGraph edge0 tgtUnitMay =
  case edge0.status of
    ReadyEdgeStatusIfaceGraph ->
      case tgtUnitMay of
        Just tgtUnit0
          | tgtUnit0.status == ReadyNodeStatusIfaceGraph
          , edge0.seenApi /= tgtUnit0.hashApi || edge0.seenInline /= tgtUnit0.hashInline ->
              [ diagValidateIfaceGraph "IFG-VAL-EDGE-SEEN"
                  ("ready interface edge " <> showTextValidateIfaceGraph edge0.uid
                    <> " has seen hashes (api=" <> showTextValidateIfaceGraph edge0.seenApi
                    <> ", inline=" <> showTextValidateIfaceGraph edge0.seenInline
                    <> ") but target node " <> showTextValidateIfaceGraph edge0.to
                    <> " currently has (api=" <> showTextValidateIfaceGraph tgtUnit0.hashApi
                    <> ", inline=" <> showTextValidateIfaceGraph tgtUnit0.hashInline
                    <> ")")
              ]
        _ -> []
    _ -> []

validateStatusEdgeIfaceGraph
  :: EdgeIfaceGraph
  -> Maybe UnitIfaceGraph
  -> Maybe UnitIfaceGraph
  -> [Diag]
validateStatusEdgeIfaceGraph edge0 srcUnitMay tgtUnitMay =
  let
    readyOk =
      isReadyUnitValidateIfaceGraph srcUnitMay
        && isReadyUnitValidateIfaceGraph tgtUnitMay
        && seenMatchValidateIfaceGraph edge0 tgtUnitMay

    staleOk =
      not (isMissingOrFailedUnitValidateIfaceGraph srcUnitMay
            || isMissingOrFailedUnitValidateIfaceGraph tgtUnitMay)
        && (isStaleUnitValidateIfaceGraph srcUnitMay
              || isStaleUnitValidateIfaceGraph tgtUnitMay)

    blockedOk =
      isMissingOrFailedUnitValidateIfaceGraph srcUnitMay
        || isMissingOrFailedUnitValidateIfaceGraph tgtUnitMay
  in
  case edge0.status of
    ReadyEdgeStatusIfaceGraph
      | not readyOk ->
          [ diagValidateIfaceGraph "IFG-VAL-EDGE-STATUS"
              ("edge " <> showTextValidateIfaceGraph edge0.uid
                <> " is marked ready but source/target units are not both ready with matching seen hashes")
          ]
      | otherwise -> []

    StaleEdgeStatusIfaceGraph
      | not staleOk ->
          [ diagValidateIfaceGraph "IFG-VAL-EDGE-STATUS"
              ("edge " <> showTextValidateIfaceGraph edge0.uid
                <> " is marked stale but source/target units are not in a legal stale combination")
          ]
      | otherwise -> []

    BlockedEdgeStatusIfaceGraph
      | not blockedOk ->
          [ diagValidateIfaceGraph "IFG-VAL-EDGE-STATUS"
              ("edge " <> showTextValidateIfaceGraph edge0.uid
                <> " is marked blocked but neither source nor target unit is missing/failed")
          ]
      | otherwise -> []

validateByFromEntryIfaceGraph :: IfaceGraph -> (NodeId, Vector IfaceEdgeId) -> [Diag]
validateByFromEntryIfaceGraph graph0 (node0, edgeIds0) =
  let
    nodeDiag =
      [ diagValidateIfaceGraph "IFG-VAL-IDX-BYFROM-NODE"
          ("byFrom index contains missing node " <> showTextValidateIfaceGraph node0)
      | lookupNodeLooseValidateIfaceGraph graph0.modGraph node0 == Nothing
      ]
    dupDiag =
      [ diagValidateIfaceGraph "IFG-VAL-IDX-BYFROM-DUP"
          ("byFrom index for node " <> showTextValidateIfaceGraph node0
            <> " contains duplicate edge id " <> showTextValidateIfaceGraph edgeId0)
      | edgeId0 <- dupValsValidateIfaceGraph (V.toList edgeIds0)
      ]
    refDiag =
      concatMap (validateByFromRefIfaceGraph graph0 node0) (V.toList edgeIds0)
  in
  nodeDiag <> dupDiag <> refDiag

validateByFromRefIfaceGraph :: IfaceGraph -> NodeId -> IfaceEdgeId -> [Diag]
validateByFromRefIfaceGraph graph0 node0 edgeId0 =
  case lookupEdgeLooseValidateIfaceGraph graph0 edgeId0 of
    Nothing ->
      [ diagValidateIfaceGraph "IFG-VAL-IDX-BYFROM-MISS"
          ("byFrom index for node " <> showTextValidateIfaceGraph node0
            <> " refers to missing edge " <> showTextValidateIfaceGraph edgeId0)
      ]

    Just edge0
      | edge0.from /= node0 ->
          [ diagValidateIfaceGraph "IFG-VAL-IDX-BYFROM-WRONG"
              ("byFrom index for node " <> showTextValidateIfaceGraph node0
                <> " contains edge " <> showTextValidateIfaceGraph edgeId0
                <> " whose source is " <> showTextValidateIfaceGraph edge0.from)
          ]
      | otherwise -> []

validateByFromCoverageIfaceGraph :: IfaceGraph -> EdgeIfaceGraph -> [Diag]
validateByFromCoverageIfaceGraph graph0 edge0 =
  let
    edgeIds0 = Map.findWithDefault V.empty edge0.from graph0.byFrom
  in
  [ diagValidateIfaceGraph "IFG-VAL-IDX-BYFROM-MISS"
      ("edge " <> showTextValidateIfaceGraph edge0.uid
        <> " is missing from byFrom index for node " <> showTextValidateIfaceGraph edge0.from)
  | not (V.elem edge0.uid edgeIds0)
  ]

validateByToEntryIfaceGraph :: IfaceGraph -> (NodeId, Vector IfaceEdgeId) -> [Diag]
validateByToEntryIfaceGraph graph0 (node0, edgeIds0) =
  let
    nodeDiag =
      [ diagValidateIfaceGraph "IFG-VAL-IDX-BYTO-NODE"
          ("byTo index contains missing node " <> showTextValidateIfaceGraph node0)
      | lookupNodeLooseValidateIfaceGraph graph0.modGraph node0 == Nothing
      ]
    dupDiag =
      [ diagValidateIfaceGraph "IFG-VAL-IDX-BYTO-DUP"
          ("byTo index for node " <> showTextValidateIfaceGraph node0
            <> " contains duplicate edge id " <> showTextValidateIfaceGraph edgeId0)
      | edgeId0 <- dupValsValidateIfaceGraph (V.toList edgeIds0)
      ]
    refDiag =
      concatMap (validateByToRefIfaceGraph graph0 node0) (V.toList edgeIds0)
  in
  nodeDiag <> dupDiag <> refDiag

validateByToRefIfaceGraph :: IfaceGraph -> NodeId -> IfaceEdgeId -> [Diag]
validateByToRefIfaceGraph graph0 node0 edgeId0 =
  case lookupEdgeLooseValidateIfaceGraph graph0 edgeId0 of
    Nothing ->
      [ diagValidateIfaceGraph "IFG-VAL-IDX-BYTO-MISS"
          ("byTo index for node " <> showTextValidateIfaceGraph node0
            <> " refers to missing edge " <> showTextValidateIfaceGraph edgeId0)
      ]

    Just edge0
      | edge0.to /= node0 ->
          [ diagValidateIfaceGraph "IFG-VAL-IDX-BYTO-WRONG"
              ("byTo index for node " <> showTextValidateIfaceGraph node0
                <> " contains edge " <> showTextValidateIfaceGraph edgeId0
                <> " whose target is " <> showTextValidateIfaceGraph edge0.to)
          ]
      | otherwise -> []

validateByToCoverageIfaceGraph :: IfaceGraph -> EdgeIfaceGraph -> [Diag]
validateByToCoverageIfaceGraph graph0 edge0 =
  let
    edgeIds0 = Map.findWithDefault V.empty edge0.to graph0.byTo
  in
  [ diagValidateIfaceGraph "IFG-VAL-IDX-BYTO-MISS"
      ("edge " <> showTextValidateIfaceGraph edge0.uid
        <> " is missing from byTo index for node " <> showTextValidateIfaceGraph edge0.to)
  | not (V.elem edge0.uid edgeIds0)
  ]

normUsesValidateIfaceGraph :: Vector UseIfaceGraph -> Vector UseIfaceGraph
normUsesValidateIfaceGraph uses0 =
  let
    uses1 = Set.toAscList (Set.fromList (V.toList uses0))
    hasAll = AllUseIfaceGraph `elem` uses1
    uses2 =
      if hasAll
        then filter keepUseAllValidateIfaceGraph uses1
        else uses1
  in
  V.fromList uses2

keepUseAllValidateIfaceGraph :: UseIfaceGraph -> Bool
keepUseAllValidateIfaceGraph use0 =
  case use0 of
    NamespaceUseIfaceGraph -> True
    AllUseIfaceGraph -> True
    _ -> False

hasDepValidateIfaceGraph :: ModGraph -> NodeId -> NodeId -> Bool
hasDepValidateIfaceGraph modGraph0 from0 to0 =
  any (matchesDepValidateIfaceGraph from0 to0) (IntMap.elems modGraph0.edges)

matchesDepValidateIfaceGraph :: NodeId -> NodeId -> EdgeModGraph -> Bool
matchesDepValidateIfaceGraph from0 to0 edge0 =
  edge0.from == from0
    && edge0.toMay == Just to0
    && allowedDepStatusValidateIfaceGraph edge0.status

allowedDepStatusValidateIfaceGraph :: StatusEdgeModGraph -> Bool
allowedDepStatusValidateIfaceGraph status0 =
  case status0 of
    ReadyEdgeStatusModGraph -> True
    SelfEdgeStatusModGraph -> True
    _ -> False

lookupNodeLooseValidateIfaceGraph :: ModGraph -> NodeId -> Maybe NodeModGraph
lookupNodeLooseValidateIfaceGraph modGraph0 node0 =
  case IntMap.lookup (keyNodeValidateIfaceGraph node0) modGraph0.nodes of
    Just node1
      | node1.uid == node0 -> Just node1
    _ ->
      find (\node1 -> node1.uid == node0) (IntMap.elems modGraph0.nodes)

lookupUnitLooseValidateIfaceGraph :: IfaceGraph -> NodeId -> Maybe UnitIfaceGraph
lookupUnitLooseValidateIfaceGraph graph0 node0 =
  case IntMap.lookup (keyNodeValidateIfaceGraph node0) graph0.units of
    Just unit0
      | unit0.node == node0 -> Just unit0
    _ ->
      find (\unit0 -> unit0.node == node0) (IntMap.elems graph0.units)

lookupEdgeLooseValidateIfaceGraph :: IfaceGraph -> IfaceEdgeId -> Maybe EdgeIfaceGraph
lookupEdgeLooseValidateIfaceGraph graph0 edgeId0 =
  case IntMap.lookup (keyIfaceEdgeValidateIfaceGraph edgeId0) graph0.edges of
    Just edge0
      | edge0.uid == edgeId0 -> Just edge0
    _ ->
      find (\edge0 -> edge0.uid == edgeId0) (IntMap.elems graph0.edges)

isReadyUnitValidateIfaceGraph :: Maybe UnitIfaceGraph -> Bool
isReadyUnitValidateIfaceGraph unitMay =
  case unitMay of
    Just unit0 -> unit0.status == ReadyNodeStatusIfaceGraph
    Nothing -> False

isStaleUnitValidateIfaceGraph :: Maybe UnitIfaceGraph -> Bool
isStaleUnitValidateIfaceGraph unitMay =
  case unitMay of
    Just unit0 -> unit0.status == StaleNodeStatusIfaceGraph
    Nothing -> False

isMissingOrFailedUnitValidateIfaceGraph :: Maybe UnitIfaceGraph -> Bool
isMissingOrFailedUnitValidateIfaceGraph unitMay =
  case unitMay of
    Nothing -> True
    Just unit0 -> unit0.status == FailedNodeStatusIfaceGraph

seenMatchValidateIfaceGraph :: EdgeIfaceGraph -> Maybe UnitIfaceGraph -> Bool
seenMatchValidateIfaceGraph edge0 tgtUnitMay =
  case tgtUnitMay of
    Just tgtUnit0
      | tgtUnit0.status == ReadyNodeStatusIfaceGraph ->
          edge0.seenApi == tgtUnit0.hashApi
            && edge0.seenInline == tgtUnit0.hashInline
    _ -> False

keyExportValidateIfaceGraph :: ExportIfaceGraph -> ExportKeyIfaceGraph
keyExportValidateIfaceGraph export0 =
  case export0 of
    ValueExportIfaceGraph name0 _ -> ValueKeyIfaceGraph name0
    TypeExportIfaceGraph name0 _ -> TypeKeyIfaceGraph name0
    CtorExportIfaceGraph name0 _ -> CtorKeyIfaceGraph name0
    AliasExportIfaceGraph name0 _ -> AliasKeyIfaceGraph name0
    EffectExportIfaceGraph name0 _ -> EffectKeyIfaceGraph name0
    ForeignExportIfaceGraph name0 _ -> ForeignKeyIfaceGraph name0

dupValsValidateIfaceGraph :: Ord a => [a] -> [a]
dupValsValidateIfaceGraph vals0 =
  Map.keys $
    Map.filter (> (1 :: Int)) $
      Map.fromListWith (+) [(val0, 1 :: Int) | val0 <- vals0]

keyNodeValidateIfaceGraph :: NodeId -> Int
keyNodeValidateIfaceGraph (NodeId raw0) = fromIntegral raw0

keyIfaceEdgeValidateIfaceGraph :: IfaceEdgeId -> Int
keyIfaceEdgeValidateIfaceGraph (IfaceEdgeId raw0) = fromIntegral raw0

-- mkDiag :: CodeDiag -> StageDiag -> SeverityDiag -> Range -> Text -> Diag

diagValidateIfaceGraph :: Text.Text -> Text.Text -> Diag
diagValidateIfaceGraph code0 msg0 =
  mkDiag (CodeDiag code0) IfaceGraphDG ErrorDS emptyRange ("Fuddle.Compiler.IfaceGraph.Validate: " <> msg0)

showTextValidateIfaceGraph :: Show a => a -> Text.Text
showTextValidateIfaceGraph = Text.pack . show