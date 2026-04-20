{- HLINT ignore "Use list comprehension" -}
module Fuddle.Compiler.ModGraph.Validate
  ( validateModGraph
  ) where

import Data.Foldable (foldl', toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V

import Fuddle.Compiler.Base.Diag (Diag(..), SeverityDiag(..), CodeDiag (..), StageDiag (..))
import Fuddle.Compiler.Base.Range (Range, emptyRange)
import Fuddle.Compiler.ModGraph.Name (ModName)
import Fuddle.Compiler.ModGraph.Origin (SourceLocMod)
import Fuddle.Compiler.ModGraph.Types ( EdgeId(..), NodeId(..), SccId(..), StatusEdgeModGraph (..), StatusNodeModGraph (..) )
import Fuddle.Compiler.ModGraph.GraphTypes (EdgeModGraph(..), ModGraph(..), NodeModGraph(..), SccModGraph(..))
import Fuddle.Compiler.ModGraph.Origin (OriginMod(..))
import Fuddle.Compiler.ModGraph.Header (ImportHdrMod(..))
import Data.Coerce (coerce)


validateModGraph :: ModGraph -> Vector Diag
validateModGraph graph =
  let
    diags =
      concat
        [ checkIdsValidateMod graph
        , checkOriginsValidateMod graph
        , checkEdgesValidateMod graph
        , checkNodeEdgesValidateMod graph
        , checkSccsValidateMod graph
        , checkIndexesValidateMod graph
        , checkIfaceHashValidateMod graph
        ]
  in
  V.fromList (List.sortOn keyDiagValidateMod diags)

checkIdsValidateMod :: ModGraph -> [Diag]
checkIdsValidateMod graph =
  concat
    [ concatMap checkNodeKeyValidateMod (IntMap.toList graph.nodes)
    , concatMap checkEdgeKeyValidateMod (IntMap.toList graph.edges)
    , concatMap checkSccKeyValidateMod (IntMap.toList graph.sccs)
    , dupNodeDiags
    , dupEdgeDiags
    , dupSccDiags
    ]
  where
    dupNodeDiags =
      concatMap mkNodeDupDiagValidateMod (Map.toAscList (bucketOnValidateMod (.uid) (nodesValidateMod graph)))

    dupEdgeDiags =
      concatMap mkEdgeDupDiagValidateMod (Map.toAscList (bucketOnValidateMod (.uid) (edgesValidateMod graph)))

    dupSccDiags =
      concatMap mkSccDupDiagValidateMod (Map.toAscList (bucketOnValidateMod (.uid) (sccsValidateMod graph)))

checkNodeKeyValidateMod :: (Int, NodeModGraph) -> [Diag]
checkNodeKeyValidateMod (key0, node)
  | key0 == keyNodeValidateMod node.uid = []
  | otherwise =
      [ diagNodeValidateMod "MGV001" node $
          "node table key " <> tshowValidateMod key0 <> " does not match node uid "
            <> tshowValidateMod node.uid
      ]

checkEdgeKeyValidateMod :: (Int, EdgeModGraph) -> [Diag]
checkEdgeKeyValidateMod (key0, edge)
  | key0 == keyEdgeValidateMod edge.uid = []
  | otherwise =
      [ diagEdgeLooseValidateMod "MGV002" Nothing edge.importHdr.rangeMay $
          "edge table key " <> tshowValidateMod key0 <> " does not match edge uid "
            <> tshowValidateMod edge.uid
      ]

checkSccKeyValidateMod :: (Int, SccModGraph) -> [Diag]
checkSccKeyValidateMod (key0, scc)
  | key0 == keySccValidateMod scc.uid = []
  | otherwise =
      [ diagSccValidateMod "MGV003" Nothing $
          "scc table key " <> tshowValidateMod key0 <> " does not match scc uid "
            <> tshowValidateMod scc.uid
      ]

mkNodeDupDiagValidateMod :: (NodeId, [NodeModGraph]) -> [Diag]
mkNodeDupDiagValidateMod (nodeId0, nodes0)
  | length nodes0 <= 1 = []
  | otherwise =
      let
        node = head nodes0
        locsTxt = renderListValidateMod (map (.origin.loc) nodes0)
      in
      [ diagNodeValidateMod "MGV004" node $
          "duplicate node uid " <> tshowValidateMod nodeId0
            <> " assigned to multiple origins "
            <> locsTxt
      ]

mkEdgeDupDiagValidateMod :: (EdgeId, [EdgeModGraph]) -> [Diag]
mkEdgeDupDiagValidateMod (edgeId0, edges0)
  | length edges0 <= 1 = []
  | otherwise =
      let
        edge = head edges0
        fromsTxt = renderListValidateMod (map (.from) edges0)
      in
      [ diagEdgeLooseValidateMod "MGV005" Nothing edge.importHdr.rangeMay $
          "duplicate edge uid " <> tshowValidateMod edgeId0
            <> " assigned to multiple source nodes "
            <> fromsTxt
      ]

mkSccDupDiagValidateMod :: (SccId, [SccModGraph]) -> [Diag]
mkSccDupDiagValidateMod (sccId0, sccs0)
  | length sccs0 <= 1 = []
  | otherwise =
      [ diagSccValidateMod "MGV006" Nothing $
          "duplicate scc uid " <> tshowValidateMod sccId0
            <> " appears multiple times in the scc table"
      ]

checkOriginsValidateMod :: ModGraph -> [Diag]
checkOriginsValidateMod graph =
  concat
    [ dupOriginDiags
    , concatMap (checkNodeByOriginValidateMod graph) (nodesValidateMod graph)
    , concatMap checkByOriginEntryValidateMod (Map.toAscList graph.byOrigin)
    ]
  where
  dupOriginDiags =
    concatMap mkOriginDupDiagValidateMod
      (Map.toAscList (bucketOnValidateMod (.origin.loc) (nodesValidateMod graph)))

checkNodeByOriginValidateMod :: ModGraph -> NodeModGraph -> [Diag]
checkNodeByOriginValidateMod graph node =
  let
    loc0 = node.origin.loc
  in
  case Map.lookup loc0 graph.byOrigin of
    Nothing ->
      [ diagNodeValidateMod "MGV010" node $
          "node origin " <> tshowValidateMod loc0
            <> " is not indexed in byOrigin"
      ]
    Just nodeId0
      | nodeId0 == node.uid -> []
      | otherwise ->
          [ diagNodeValidateMod "MGV011" node $
              "byOrigin maps origin " <> tshowValidateMod loc0
                <> " to node " <> tshowValidateMod nodeId0
                <> " instead of node " <> tshowValidateMod node.uid
          ]

checkByOriginEntryValidateMod :: (SourceLocMod, NodeId) -> [Diag]
checkByOriginEntryValidateMod (loc0, nodeId0) =
  case lookupNodeValidateMod Nothing nodeId0 of
    Nothing ->
      [ diagGraphValidateMod "MGV012" (Just loc0) Nothing $
          "byOrigin entry for " <> tshowValidateMod loc0
            <> " points to missing node " <> tshowValidateMod nodeId0
      ]
    Just node
      | node.origin.loc == loc0 -> []
      | otherwise ->
          [ diagNodeValidateMod "MGV013" node $
              "byOrigin key " <> tshowValidateMod loc0
                <> " does not match node origin "
                <> tshowValidateMod node.origin.loc
          ]
  where
    lookupNodeValidateMod :: Maybe ModGraph -> NodeId -> Maybe NodeModGraph
    lookupNodeValidateMod _ = const Nothing

mkOriginDupDiagValidateMod :: (SourceLocMod, [NodeModGraph]) -> [Diag]
mkOriginDupDiagValidateMod (loc0, nodes0)
  | length nodes0 <= 1 = []
  | otherwise =
      let
        nodeIdsTxt = renderListValidateMod (map (.uid) nodes0)
      in
      [ diagNodeValidateMod "MGV014" (head nodes0) $
          "multiple nodes share the same origin " <> tshowValidateMod loc0
            <> ": " <> nodeIdsTxt
      ]

checkEdgesValidateMod :: ModGraph -> [Diag]
checkEdgesValidateMod graph =
  concatMap (checkEdgeValidateMod graph) (edgesValidateMod graph)

checkEdgeValidateMod :: ModGraph -> EdgeModGraph -> [Diag]
checkEdgeValidateMod graph edge =
  concat
    [ checkEdgeImportUidValidateMod edge
    , checkEdgeSourceValidateMod graph edge
    , checkEdgeTargetValidateMod graph edge
    , checkEdgeStatusValidateMod graph edge
    ]

checkEdgeImportUidValidateMod :: EdgeModGraph -> [Diag]
checkEdgeImportUidValidateMod edge
  | edge.importHdr.uid == edge.uid = []
  | otherwise =
      [ diagEdgeLooseValidateMod "MGV020" Nothing edge.importHdr.rangeMay $
          "edge uid " <> tshowValidateMod edge.uid
            <> " does not match import header uid "
            <> tshowValidateMod edge.importHdr.uid
      ]

checkEdgeSourceValidateMod :: ModGraph -> EdgeModGraph -> [Diag]
checkEdgeSourceValidateMod graph edge =
  case lookupNodeModValidateMod graph edge.from of
    Just _ -> []
    Nothing ->
      [ diagEdgeLooseValidateMod "MGV021" Nothing edge.importHdr.rangeMay $
          "edge " <> tshowValidateMod edge.uid
            <> " references missing source node "
            <> tshowValidateMod edge.from
      ]

checkEdgeTargetValidateMod :: ModGraph -> EdgeModGraph -> [Diag]
checkEdgeTargetValidateMod graph edge =
  case edge.toMay of
    Nothing -> []
    Just nodeId0 ->
      case lookupNodeModValidateMod graph nodeId0 of
        Just _ -> []
        Nothing ->
          [ diagEdgeValidateMod graph "MGV022" edge $
              "edge " <> tshowValidateMod edge.uid
                <> " references missing target node "
                <> tshowValidateMod nodeId0
          ]

checkEdgeStatusValidateMod :: ModGraph -> EdgeModGraph -> [Diag]
checkEdgeStatusValidateMod graph edge =
  case edge.status of
    ReadyEdgeStatusModGraph ->
      case edge.toMay of
        Nothing ->
          [ diagEdgeValidateMod graph "MGV023" edge $
              "ready edge " <> tshowValidateMod edge.uid
                <> " must have a concrete target"
          ]
        Just nodeId0
          | nodeId0 == edge.from ->
              [ diagEdgeValidateMod graph "MGV024" edge $
                  "ready edge " <> tshowValidateMod edge.uid
                    <> " targets its own source node and must be marked self"
              ]
          | otherwise -> []

    MissingEdgeStatusModGraph ->
      case edge.toMay of
        Nothing -> []
        Just nodeId0 ->
          [ diagEdgeValidateMod graph "MGV025" edge $
              "missing edge " <> tshowValidateMod edge.uid
                <> " must not carry a target, but has "
                <> tshowValidateMod nodeId0
          ]

    AmbiguousEdgeStatusModGraph ->
      case edge.toMay of
        Nothing -> []
        Just nodeId0 ->
          [ diagEdgeValidateMod graph "MGV026" edge $
              "ambiguous edge " <> tshowValidateMod edge.uid
                <> " must not carry a target, but has "
                <> tshowValidateMod nodeId0
          ]

    HiddenEdgeStatusModGraph ->
      case edge.toMay of
        Nothing ->
          [ diagEdgeValidateMod graph "MGV027" edge $
              "hidden edge " <> tshowValidateMod edge.uid
                <> " must retain its hidden target"
          ]
        Just nodeId0
          | nodeId0 == edge.from ->
              [ diagEdgeValidateMod graph "MGV028" edge $
                  "hidden edge " <> tshowValidateMod edge.uid
                    <> " targets its own source node and must be marked self"
              ]
          | otherwise -> []

    SelfEdgeStatusModGraph ->
      case edge.toMay of
        Just nodeId0
          | nodeId0 == edge.from -> []
          | otherwise ->
              [ diagEdgeValidateMod graph "MGV029" edge $
                  "self edge " <> tshowValidateMod edge.uid
                    <> " must target its own source node "
                    <> tshowValidateMod edge.from
                    <> ", but targets "
                    <> tshowValidateMod nodeId0
              ]
        Nothing ->
          [ diagEdgeValidateMod graph "MGV030" edge $
              "self edge " <> tshowValidateMod edge.uid
                <> " must have a concrete target equal to its source node"
          ]

checkNodeEdgesValidateMod :: ModGraph -> [Diag]
checkNodeEdgesValidateMod graph =
  concatMap (checkNodeEdgeRefsValidateMod graph) (nodesValidateMod graph)
    <> concatMap (checkNodeEdgeSetsValidateMod graph) (nodesValidateMod graph)

checkNodeEdgeRefsValidateMod :: ModGraph -> NodeModGraph -> [Diag]
checkNodeEdgeRefsValidateMod graph node =
  concat
    [ concatMap checkImportRefValidateMod (toList node.imports)
    , concatMap checkImportedByRefValidateMod (toList node.importedBy)
    ]
  where
    checkImportRefValidateMod edgeId0 =
      case lookupEdgeModValidateMod graph edgeId0 of
        Nothing ->
          [ diagNodeValidateMod "MGV031" node $
              "node imports references missing edge " <> tshowValidateMod edgeId0
          ]
        Just edge
          | edge.from == node.uid -> []
          | otherwise ->
              [ diagNodeValidateMod "MGV032" node $
                  "node imports references edge " <> tshowValidateMod edgeId0
                    <> " whose source is " <> tshowValidateMod edge.from
                    <> " instead of " <> tshowValidateMod node.uid
              ]

    checkImportedByRefValidateMod edgeId0 =
      case lookupEdgeModValidateMod graph edgeId0 of
        Nothing ->
          [ diagNodeValidateMod "MGV033" node $
              "node importedBy references missing edge " <> tshowValidateMod edgeId0
          ]
        Just edge
          | edge.toMay == Just node.uid -> []
          | otherwise ->
              [ diagNodeValidateMod "MGV034" node $
                  "node importedBy references edge " <> tshowValidateMod edgeId0
                    <> " whose target is " <> tshowValidateMod edge.toMay
                    <> " instead of " <> tshowValidateMod (Just node.uid)
              ]

checkNodeEdgeSetsValidateMod :: ModGraph -> NodeModGraph -> [Diag]
checkNodeEdgeSetsValidateMod graph node =
  concat
    [ checkImportsSetValidateMod node expectedImportsSet
    , checkImportedBySetValidateMod node expectedImportedBySet
    ]
  where
    expectedImportsSet = Map.findWithDefault Set.empty node.uid (importsByNodeValidateMod graph)
    expectedImportedBySet = Map.findWithDefault Set.empty node.uid (importedByNodeValidateMod graph)

checkImportsSetValidateMod :: NodeModGraph -> Set EdgeId -> [Diag]
checkImportsSetValidateMod node expectedSet
  | not hasDup && storedSet == expectedSet = []
  | otherwise =
      [ diagNodeValidateMod "MGV035" node $
          "node imports mismatch, stored="
            <> renderListValidateMod (toList node.imports)
            <> ", expected="
            <> renderSetValidateMod expectedSet
      ]
  where
    storedSet = Set.fromList (toList node.imports)
    hasDup = vectorHasDupValidateMod node.imports

checkImportedBySetValidateMod :: NodeModGraph -> Set EdgeId -> [Diag]
checkImportedBySetValidateMod node expectedSet
  | not hasDup && storedSet == expectedSet = []
  | otherwise =
      [ diagNodeValidateMod "MGV036" node $
          "node importedBy mismatch, stored="
            <> renderListValidateMod (toList node.importedBy)
            <> ", expected="
            <> renderSetValidateMod expectedSet
      ]
  where
    storedSet = Set.fromList (toList node.importedBy)
    hasDup = vectorHasDupValidateMod node.importedBy

checkSccsValidateMod :: ModGraph -> [Diag]
checkSccsValidateMod graph =
  concat
    [ concatMap (checkSccMembersValidateMod graph) (sccsValidateMod graph)
    , concatMap (checkNodeSccValidateMod membershipMap graph) (nodesValidateMod graph)
    , concatMap (checkSccDepsValidateMod graph) (sccsValidateMod graph)
    , concatMap (checkSccIndegreeValidateMod incomingMap) (sccsValidateMod graph)
    , checkTopoUniqValidateMod graph
    , concatMap (checkSccTopoValidateMod graph) (sccsValidateMod graph)
    ]
  where
    membershipMap = membershipsValidateMod graph
    incomingMap = incomingCountsValidateMod graph

checkSccMembersValidateMod :: ModGraph -> SccModGraph -> [Diag]
checkSccMembersValidateMod graph scc =
  let
    dupDiag = if vectorHasDupValidateMod scc.nodes then
        [ diagSccFromGraphValidateMod graph "MGV040" scc $
            "scc " <> tshowValidateMod scc.uid
              <> " contains duplicate node ids "
              <> renderListValidateMod (toList scc.nodes)
        ]
      else
        []
  in
  dupDiag <> concatMap checkMemberValidateMod (toList scc.nodes)
  where
    checkMemberValidateMod nodeId0 =
      case lookupNodeModValidateMod graph nodeId0 of
        Nothing ->
          [ diagSccFromGraphValidateMod graph "MGV041" scc $
              "scc " <> tshowValidateMod scc.uid
                <> " references missing node " <> tshowValidateMod nodeId0
          ]
        Just node
          | node.scc == scc.uid -> []
          | otherwise ->
              [ diagNodeValidateMod "MGV042" node $
                  "node " <> tshowValidateMod node.uid
                    <> " records scc " <> tshowValidateMod node.scc
                    <> " but appears in scc " <> tshowValidateMod scc.uid
              ]

checkNodeSccValidateMod :: Map NodeId [SccId] -> ModGraph -> NodeModGraph -> [Diag]
checkNodeSccValidateMod membershipMap graph node =
  let
    existsDiag =
      case lookupSccModValidateMod graph node.scc of
        Nothing ->
          [ diagNodeValidateMod "MGV043" node $
              "node " <> tshowValidateMod node.uid
                <> " references missing scc " <> tshowValidateMod node.scc
          ]
        Just _ -> []
  in
  existsDiag <> case Map.lookup node.uid membershipMap of
    Nothing ->
      [ diagNodeValidateMod "MGV044" node $
          "node " <> tshowValidateMod node.uid
            <> " does not belong to any scc"
      ]
    Just sccIds0
      | length sccIds0 > 1 ->
          [ diagNodeValidateMod "MGV045" node $
              "node " <> tshowValidateMod node.uid
                <> " belongs to multiple sccs "
                <> renderListValidateMod sccIds0
          ]
      | sccIds0 == [node.scc] -> []
      | otherwise ->
          [ diagNodeValidateMod "MGV046" node $
              "node " <> tshowValidateMod node.uid
                <> " membership scc "
                <> renderListValidateMod sccIds0
                <> " disagrees with node.scc "
                <> tshowValidateMod node.scc
          ]

checkSccDepsValidateMod :: ModGraph -> SccModGraph -> [Diag]
checkSccDepsValidateMod graph scc =
  let
    dupDiag =
      if vectorHasDupValidateMod scc.deps
        then
          [ diagSccFromGraphValidateMod graph "MGV047" scc $
              "scc " <> tshowValidateMod scc.uid
                <> " contains duplicate dependency ids "
                <> renderListValidateMod (toList scc.deps)
          ]
        else []
  in
  dupDiag <> concatMap checkDepValidateMod (toList scc.deps)
  where
    checkDepValidateMod depId0
      | depId0 == scc.uid =
          [ diagSccFromGraphValidateMod graph "MGV048" scc $
              "scc " <> tshowValidateMod scc.uid
                <> " contains an invalid self dependency"
          ]
      | otherwise =
          case lookupSccModValidateMod graph depId0 of
            Just _ -> []
            Nothing ->
              [ diagSccFromGraphValidateMod graph "MGV049" scc $
                  "scc " <> tshowValidateMod scc.uid
                    <> " depends on missing scc " <> tshowValidateMod depId0
              ]

checkSccIndegreeValidateMod :: Map SccId Int -> SccModGraph -> [Diag]
checkSccIndegreeValidateMod incomingMap scc
  | scc.indegree == expectedIndegree && scc.indegree >= 0 = []
  | otherwise =
      [ diagSccValidateMod "MGV050" Nothing $
          "scc " <> tshowValidateMod scc.uid
            <> " indegree is " <> tshowValidateMod scc.indegree
            <> ", expected " <> tshowValidateMod expectedIndegree
      ]
  where
    expectedIndegree = Map.findWithDefault 0 scc.uid incomingMap

checkTopoUniqValidateMod :: ModGraph -> [Diag]
checkTopoUniqValidateMod graph =
  concatMap mkDupTopoDiagValidateMod (Map.toAscList topoBuckets)
  where
    topoBuckets = bucketOnValidateMod (.topoIx) (sccsValidateMod graph)

mkDupTopoDiagValidateMod :: (Int, [SccModGraph]) -> [Diag]
mkDupTopoDiagValidateMod (topoIx0, sccs0)
  | length sccs0 <= 1 = []
  | otherwise =
      [ diagSccValidateMod "MGV051" Nothing $
          "multiple sccs share topoIx " <> tshowValidateMod topoIx0
            <> ": " <> renderListValidateMod (map (.uid) sccs0)
      ]

checkSccTopoValidateMod :: ModGraph -> SccModGraph -> [Diag]
checkSccTopoValidateMod graph scc =
  negDiag <> concatMap checkDepOrderValidateMod (toList scc.deps)
  where
    negDiag
      | scc.topoIx < 0 =
          [ diagSccFromGraphValidateMod graph "MGV052" scc $
              "scc " <> tshowValidateMod scc.uid
                <> " has negative topoIx " <> tshowValidateMod scc.topoIx
          ]
      | otherwise = []

    checkDepOrderValidateMod depId0 =
      case lookupSccModValidateMod graph depId0 of
        Nothing -> []
        Just depScc
          | depScc.topoIx < scc.topoIx -> []
          | otherwise ->
              [ diagSccFromGraphValidateMod graph "MGV053" scc $
                  "scc " <> tshowValidateMod scc.uid
                    <> " depends on scc " <> tshowValidateMod depId0
                    <> " with topoIx " <> tshowValidateMod depScc.topoIx
                    <> ", which is not earlier than "
                    <> tshowValidateMod scc.topoIx
              ]

checkIndexesValidateMod :: ModGraph -> [Diag]
checkIndexesValidateMod graph =
  concat
    [ concatMap (checkNodeByNameValidateMod graph) (nodesValidateMod graph)
    , concatMap (checkByNameEntryValidateMod graph) (Map.toAscList graph.byName)
    , concatMap (checkByOriginEntryGraphValidateMod graph) (Map.toAscList graph.byOrigin)
    ]
  where
    checkByOriginEntryGraphValidateMod graph0 (loc0, nodeId0) =
      case lookupNodeModValidateMod graph0 nodeId0 of
        Nothing ->
          [ diagGraphValidateMod "MGV054" (Just loc0) Nothing $
              "byOrigin contains missing node " <> tshowValidateMod nodeId0
            <> " for origin " <> tshowValidateMod loc0
          ]
        Just node
          | node.origin.loc == loc0 -> []
          | otherwise ->
              [ diagNodeValidateMod "MGV055" node $
                  "byOrigin entry key " <> tshowValidateMod loc0
                    <> " does not match node origin "
                    <> tshowValidateMod node.origin.loc
              ]

checkNodeByNameValidateMod :: ModGraph -> NodeModGraph -> [Diag]
checkNodeByNameValidateMod graph node =
  let
    nodeIds0 = Map.findWithDefault V.empty node.name graph.byName
  in
  if node.uid `elemVectorValidateMod` nodeIds0
    then []
    else
      [ diagNodeValidateMod "MGV060" node $
          "node " <> tshowValidateMod node.uid
            <> " with module name " <> tshowValidateMod node.name
            <> " is not indexed in byName"
      ]

checkByNameEntryValidateMod :: ModGraph -> (ModName, Vector NodeId) -> [Diag]
checkByNameEntryValidateMod graph (modName0, nodeIds0) =
  concat
    [ dupDiag
    , concatMap checkEntryValidateMod (toList nodeIds0)
    , membershipDiag
    ]
  where
    dupDiag
      | vectorHasDupValidateMod nodeIds0 =
          [ diagGraphValidateMod "MGV061" Nothing Nothing $
              "byName entry for " <> tshowValidateMod modName0
                <> " contains duplicate node ids "
                <> renderListValidateMod (toList nodeIds0)
          ]
      | otherwise = []

    checkEntryValidateMod nodeId0 =
      case lookupNodeModValidateMod graph nodeId0 of
        Nothing ->
          [ diagGraphValidateMod "MGV062" Nothing Nothing $
              "byName entry for " <> tshowValidateMod modName0
                <> " references missing node " <> tshowValidateMod nodeId0
          ]
        Just node
          | node.name == modName0 -> []
          | otherwise ->
              [ diagNodeValidateMod "MGV063" node $
                  "byName key " <> tshowValidateMod modName0
                    <> " does not match node name "
                    <> tshowValidateMod node.name
              ]

    storedSet = Set.fromList (toList nodeIds0)
    actualSet = Map.findWithDefault Set.empty modName0 (byNameActualValidateMod graph)

    membershipDiag
      | storedSet == actualSet = []
      | otherwise =
          [ diagGraphValidateMod "MGV064" Nothing Nothing $
              "byName membership mismatch for " <> tshowValidateMod modName0
                <> ", stored=" <> renderSetValidateMod storedSet
                <> ", actual=" <> renderSetValidateMod actualSet
          ]

checkIfaceHashValidateMod :: ModGraph -> [Diag]
checkIfaceHashValidateMod graph =
  concatMap checkNodeValidateMod (nodesValidateMod graph)
  where
    checkNodeValidateMod node
      | node.status == ReadyNodeStatusModGraph = []
      | isJust node.hashIfaceMay =
          [ diagNodeValidateMod "MGV070" node $
              "non-ready node " <> tshowValidateMod node.uid
                <> " must not carry hashIfaceMay"
          ]
      | otherwise = []

-- `hashHeader` is strict on `NodeModGraph`, so presence is already enforced by the
-- data model itself. The validator still checks the complementary rule that
-- interface hashes only appear on semantically ready nodes.

nodesValidateMod :: ModGraph -> [NodeModGraph]
nodesValidateMod graph = IntMap.elems graph.nodes

edgesValidateMod :: ModGraph -> [EdgeModGraph]
edgesValidateMod graph = IntMap.elems graph.edges

sccsValidateMod :: ModGraph -> [SccModGraph]
sccsValidateMod graph = IntMap.elems graph.sccs

lookupNodeModValidateMod :: ModGraph -> NodeId -> Maybe NodeModGraph
lookupNodeModValidateMod graph nodeId0 = IntMap.lookup (keyNodeValidateMod nodeId0) graph.nodes

lookupEdgeModValidateMod :: ModGraph -> EdgeId -> Maybe EdgeModGraph
lookupEdgeModValidateMod graph edgeId0 = IntMap.lookup (keyEdgeValidateMod edgeId0) graph.edges

lookupSccModValidateMod :: ModGraph -> SccId -> Maybe SccModGraph
lookupSccModValidateMod graph sccId0 = IntMap.lookup (keySccValidateMod sccId0) graph.sccs

importsByNodeValidateMod :: ModGraph -> Map NodeId (Set EdgeId)
importsByNodeValidateMod graph =
  foldl' step Map.empty (edgesValidateMod graph)
  where
  step :: Map NodeId (Set EdgeId) -> EdgeModGraph -> Map NodeId (Set EdgeId)
  step acc edge = Map.insertWith Set.union edge.from (Set.singleton edge.uid) acc


importedByNodeValidateMod :: ModGraph -> Map NodeId (Set EdgeId)
importedByNodeValidateMod graph =
  foldl' step Map.empty (edgesValidateMod graph)
  where
  step :: Map NodeId (Set EdgeId) -> EdgeModGraph -> Map NodeId (Set EdgeId)
  step acc edge = case edge.toMay of
    Nothing -> acc
    Just nodeId0 -> Map.insertWith Set.union nodeId0 (Set.singleton edge.uid) acc


membershipsValidateMod :: ModGraph -> Map NodeId [SccId]
membershipsValidateMod graph =
  foldl' step Map.empty (sccsValidateMod graph)
  where
  step :: Map NodeId [SccId] -> SccModGraph -> Map NodeId [SccId]
  step acc scc = foldl' (
      \acc1 nodeId0 -> Map.insertWith (flip (<>)) nodeId0 [scc.uid] acc1
    ) acc (toList scc.nodes)


incomingCountsValidateMod :: ModGraph -> Map SccId Int
incomingCountsValidateMod graph =
  foldl' step Map.empty (sccsValidateMod graph)
  where
  step :: Map SccId Int -> SccModGraph -> Map SccId Int
  step acc scc =
    let
      deps0 = Set.toAscList (Set.delete scc.uid (Set.fromList (toList scc.deps)))
    in
    foldl' (\acc1 depId0 -> Map.insertWith (+) depId0 1 acc1) acc deps0


byNameActualValidateMod :: ModGraph -> Map ModName (Set NodeId)
byNameActualValidateMod graph =
  foldl' step Map.empty (nodesValidateMod graph)
  where
  step :: Map ModName (Set NodeId) -> NodeModGraph -> Map ModName (Set NodeId)
  step acc node = Map.insertWith Set.union node.name (Set.singleton node.uid) acc

bucketOnValidateMod :: Ord k => (a -> k) -> [a] -> Map k [a]
bucketOnValidateMod keyOf =
  foldl' (\acc val -> Map.insertWith (flip (<>)) (keyOf val) [val] acc) Map.empty

vectorHasDupValidateMod :: Ord a => Vector a -> Bool
vectorHasDupValidateMod vals = Set.size (Set.fromList (toList vals)) /= V.length vals

elemVectorValidateMod :: Eq a => a -> Vector a -> Bool
elemVectorValidateMod needle = any (== needle) . toList

diagNodeValidateMod :: Text -> NodeModGraph -> Text -> Diag
diagNodeValidateMod code0 node msg0 =
  mkDiagValidateMod code0 msg0 (Just node.origin.loc) Nothing

diagEdgeValidateMod :: ModGraph -> Text -> EdgeModGraph -> Text -> Diag
diagEdgeValidateMod graph code0 edge msg0 =
  let
    locMay = do
      node <- lookupNodeModValidateMod graph edge.from
      pure node.origin.loc
  in
  mkDiagValidateMod code0 msg0 locMay edge.importHdr.rangeMay

diagEdgeLooseValidateMod :: Text -> Maybe SourceLocMod -> Maybe Range -> Text -> Diag
diagEdgeLooseValidateMod code0 locMay rangeMay0 msg0 =
  mkDiagValidateMod code0 msg0 locMay rangeMay0

diagSccValidateMod :: Text -> Maybe SourceLocMod -> Text -> Diag
diagSccValidateMod code0 locMay msg0 =
  mkDiagValidateMod code0 msg0 locMay Nothing

diagSccFromGraphValidateMod :: ModGraph -> Text -> SccModGraph -> Text -> Diag
diagSccFromGraphValidateMod graph code0 scc msg0 =
  let
    locMay = firstSccLocValidateMod graph scc
  in
  mkDiagValidateMod code0 msg0 locMay Nothing

diagGraphValidateMod :: Text -> Maybe SourceLocMod -> Maybe Range -> Text -> Diag
diagGraphValidateMod code0 locMay rangeMay0 msg0 = mkDiagValidateMod code0 msg0 locMay rangeMay0

firstSccLocValidateMod :: ModGraph -> SccModGraph -> Maybe SourceLocMod
firstSccLocValidateMod graph scc =
  foldl' pickNothing Nothing (toList scc.nodes)
  where
    pickNothing acc nodeId0 =
      case acc of
        Just _ -> acc
        Nothing -> do
          node <- lookupNodeModValidateMod graph nodeId0
          pure node.origin.loc

mkDiagValidateMod :: Text -> Text -> Maybe SourceLocMod -> Maybe Range -> Diag
mkDiagValidateMod code0 msg0 locMay rangeMay0 =
  Diag
    { severityDG = ErrorDS
    , codeDG = CodeDiag code0
    , msgDG = msg0
    , stageDG = ModuleGraphDG
    , rangeDG = fromMaybe emptyRange rangeMay0
    , relatedDG = V.empty
    , fixesDG = V.empty
    , notesDG = V.empty
    }

-- TODO: improve the Diag definition to take source loc, use Maybe Range:
keyDiagValidateMod :: Diag -> (Maybe SourceLocMod, Maybe Range, Text)
keyDiagValidateMod diag = (Nothing, Just diag.rangeDG, coerce diag.codeDG)

keyNodeValidateMod :: NodeId -> Int
keyNodeValidateMod (NodeId n) = fromIntegral n

keyEdgeValidateMod :: EdgeId -> Int
keyEdgeValidateMod (EdgeId n) = fromIntegral n

keySccValidateMod :: SccId -> Int
keySccValidateMod (SccId n) = fromIntegral n

tshowValidateMod :: Show a => a -> Text
tshowValidateMod = Text.pack . show

renderListValidateMod :: Show a => [a] -> Text
renderListValidateMod vals = "[" <> Text.intercalate ", " (map tshowValidateMod vals) <> "]"

renderSetValidateMod :: Show a => Set a -> Text
renderSetValidateMod = renderListValidateMod . Set.toAscList