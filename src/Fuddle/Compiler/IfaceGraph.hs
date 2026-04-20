{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore "Use list comprehension" -}

module Fuddle.Compiler.IfaceGraph
  ( IfaceGraphId(..)
  , IfaceEdgeId(..)

  , StatusNodeIfaceGraph(..)
  , StatusEdgeIfaceGraph(..)
  , KindDepIfaceGraph(..)
  , StageFrontIfaceGraph(..)

  , MetaIfaceGraph(..)
  , UnitIfaceGraph(..)
  , EdgeIfaceGraph(..)
  , IfaceGraph(..)

  , BuildReqIfaceGraph(..)
  , BuildResIfaceGraph(..)
  , UpdateReqIfaceGraph(..)
  , UpdateResIfaceGraph(..)

  , DeltaIfaceGraph(..)
  , ChangeExportIfaceGraph(..)

  , FrontierIfaceGraph(..)
  , PlanInvalidateIfaceGraph(..)
  , ReasonInvalidateIfaceGraph(..)
  , MarkInvalidateIfaceGraph(..)

  , ErrBuildIfaceGraph(..)
  , buildIfaceGraph
  , updateIfaceGraph
  , validateIfaceGraph
  ) where

import Control.Monad (when)

import Data.Bits (xor)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as V

import Fuddle.Compiler.Base.Core (Hash64(..))
import Fuddle.Compiler.Base.Diag (Diag, mkDiag, CodeDiag (..), StageDiag (..), SeverityDiag (..))
import Fuddle.Compiler.ModGraph ( EdgeId(..), ModGraph(..), NodeId(..), SccId(..) )
import Fuddle.Compiler.IfaceGraph.Types (
    IfaceGraphId(..), IfaceEdgeId(..), StatusNodeIfaceGraph(..)
    , StatusEdgeIfaceGraph(..), KindDepIfaceGraph(..), StageFrontIfaceGraph(..)
    , MetaIfaceGraph(..), UnitIfaceGraph(..), EdgeIfaceGraph(..), IfaceGraph(..)
    , BuildResIfaceGraph(..), UpdateReqIfaceGraph(..)
    , UseIfaceGraph(..), DepUseIfaceGraph(..), UseSummaryIfaceGraph(..)
  )
import Fuddle.Compiler.IfaceGraph.Export (ExportKeyIfaceGraph (..), ExportIfaceGraph (..))
import Fuddle.Compiler.Base.Range (emptyRange)
import Fuddle.Compiler.ModGraph.GraphTypes (
    ModGraph(..), NodeModGraph(..), SccModGraph(..), EdgeModGraph(..)
  )


data BuildReqIfaceGraph = BuildReqIfaceGraph
  { modGraph :: !ModGraph
  , units :: !(Vector UnitIfaceGraph)
  , uses :: !(Vector UseSummaryIfaceGraph)
  }
  deriving stock (Eq, Show)


data UpdateResIfaceGraph = UpdateResIfaceGraph
  { graph :: !IfaceGraph
  , deltas :: !(Vector DeltaIfaceGraph)
  , plan :: !PlanInvalidateIfaceGraph
  , diags :: !(Vector Diag)
  }
  deriving stock (Eq, Show)

data ChangeExportIfaceGraph
  = AddExportIfaceGraph !ExportKeyIfaceGraph !Hash64
  | RemoveExportIfaceGraph !ExportKeyIfaceGraph !Hash64
  | UpdateExportIfaceGraph !ExportKeyIfaceGraph !Hash64 !Hash64
  deriving stock (Eq, Show)

data DeltaIfaceGraph = DeltaIfaceGraph
  { node :: !NodeId
  , apiChanged :: !Bool
  , namespaceChanged :: !Bool
  , inlineChanged :: !Bool
  , exportsChanged :: !(Vector ChangeExportIfaceGraph)
  }
  deriving stock (Eq, Show)

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

data ErrBuildIfaceGraph
  = MissingNodeErrBuildIfaceGraph !NodeId
  | DuplicateUnitErrBuildIfaceGraph !NodeId
  | DuplicateUseErrBuildIfaceGraph !NodeId
  | InvalidTargetErrBuildIfaceGraph !NodeId !NodeId
  | InvariantErrBuildIfaceGraph !Text
  deriving stock (Eq, Show)

buildIfaceGraph :: BuildReqIfaceGraph -> Either ErrBuildIfaceGraph BuildResIfaceGraph
buildIfaceGraph req = do
  unitsIn <- unitsInputIfaceGraph req.modGraph req.units
  usesIn <- usesInputIfaceGraph req.modGraph req.uses
  graph0 <- mkIfaceGraphIfaceGraph 1 req.modGraph unitsIn usesIn
  let
    diags0 = validateIfaceGraph graph0
  pure BuildResIfaceGraph { graph = graph0, diags = diags0 }

updateIfaceGraph :: UpdateReqIfaceGraph -> Either ErrBuildIfaceGraph UpdateResIfaceGraph
updateIfaceGraph req = do
  unitsUpdIn <- unitsInputIfaceGraph req.modGraph req.unitsUpd
  usesUpdIn <- usesInputIfaceGraph req.modGraph req.usesUpd
  let
    (usesBase, staleNodesUse) = usesBaseIfaceGraph req.prev req.modGraph
    unitsBase = unitsBaseIfaceGraph req.prev req.modGraph staleNodesUse
    unitsIn = Map.union unitsUpdIn unitsBase
    usesIn = Map.union usesUpdIn usesBase
    ver = req.prev.meta.version + 1
  graph0 <- mkIfaceGraphIfaceGraph ver req.modGraph unitsIn usesIn
  let
    deltas0 = deltasUpdIfaceGraph req.prev graph0 unitsUpdIn
    refreshed0 = refreshedNodesIfaceGraph unitsUpdIn usesUpdIn
    plan0 = planIfaceGraph graph0 deltas0 refreshed0
    diags0 = validateIfaceGraph graph0
  pure UpdateResIfaceGraph { graph = graph0, deltas = deltas0, plan = plan0, diags = diags0 }

validateIfaceGraph :: IfaceGraph -> Vector Diag
validateIfaceGraph graph =
  let diagsMeta = validateMetaIfaceGraph graph
      diagsUnits = validateUnitsIfaceGraph graph
      diagsEdges = validateEdgesIfaceGraph graph
      diagsIx = validateIndexesIfaceGraph graph
  in V.fromList (diagsMeta <> diagsUnits <> diagsEdges <> diagsIx)

unitsInputIfaceGraph :: ModGraph -> Vector UnitIfaceGraph -> Either ErrBuildIfaceGraph (Map NodeId UnitIfaceGraph)
unitsInputIfaceGraph modGraph0 units0 =
  foldl' step (Right Map.empty) (V.toList units0)
  where
    step acc unit0 = do
      units1 <- acc
      if not (nodeExistsIfaceGraph modGraph0 unit0.node)
        then Left (MissingNodeErrBuildIfaceGraph unit0.node)
        else if Map.member unit0.node units1
          then Left (DuplicateUnitErrBuildIfaceGraph unit0.node)
          else pure (Map.insert unit0.node (normalizeUnitIfaceGraph modGraph0 unit0) units1)

usesInputIfaceGraph :: ModGraph -> Vector UseSummaryIfaceGraph -> Either ErrBuildIfaceGraph (Map NodeId UseSummaryIfaceGraph)
usesInputIfaceGraph modGraph0 uses0 =
  foldl' step (Right Map.empty) (V.toList uses0)
  where
    step acc use0 = do
      uses1 <- acc
      let use1 = normalizeUseSummaryIfaceGraph use0
      if not (nodeExistsIfaceGraph modGraph0 use1.from)
        then Left (MissingNodeErrBuildIfaceGraph use1.from)
        else if Map.member use1.from uses1
          then Left (DuplicateUseErrBuildIfaceGraph use1.from)
          else do
            validateDepsIfaceGraph modGraph0 use1
            pure (Map.insert use1.from use1 uses1)

validateDepsIfaceGraph :: ModGraph -> UseSummaryIfaceGraph -> Either ErrBuildIfaceGraph ()
validateDepsIfaceGraph modGraph0 use0 =
  foldl' step (Right ()) (V.toList use0.deps)
  where
  step :: Either ErrBuildIfaceGraph () -> DepUseIfaceGraph -> Either ErrBuildIfaceGraph ()
  step acc dep0 = do
    _ <- acc
    when (not (nodeExistsIfaceGraph modGraph0 dep0.to) || not (depExistsIfaceGraph modGraph0 use0.from dep0.to)) $ Left (InvalidTargetErrBuildIfaceGraph use0.from dep0.to)


mkIfaceGraphIfaceGraph
  :: Word64
  -> ModGraph
  -> Map NodeId UnitIfaceGraph
  -> Map NodeId UseSummaryIfaceGraph
  -> Either ErrBuildIfaceGraph IfaceGraph
mkIfaceGraphIfaceGraph ver modGraph0 unitsIn usesIn = do
  let units1 = unitsFullIfaceGraph modGraph0 unitsIn
  let edges1 = edgesFullIfaceGraph units1 usesIn
  let byFrom1 = indexByFromIfaceGraph edges1
  let byTo1 = indexByToIfaceGraph edges1
  let meta1 = metaIfaceGraph ver modGraph0 units1 edges1
  pure IfaceGraph
    { meta = meta1
    , modGraph = modGraph0
    , units = units1
    , edges = edges1
    , byFrom = byFrom1
    , byTo = byTo1
    }

unitsFullIfaceGraph :: ModGraph -> Map NodeId UnitIfaceGraph -> IntMap UnitIfaceGraph
unitsFullIfaceGraph modGraph0 unitsIn =
  IntMap.fromList
    [ (keyNodeIdIfaceGraph nodeId0, fromMaybe (staleUnitIfaceGraph modGraph0 nodeId0) (Map.lookup nodeId0 unitsIn))
    | nodeId0 <- nodeIdsIfaceGraph modGraph0
    ]

edgesFullIfaceGraph :: IntMap UnitIfaceGraph -> Map NodeId UseSummaryIfaceGraph -> IntMap EdgeIfaceGraph
edgesFullIfaceGraph units0 usesIn =
  let seeds0 = concatMap edgeSeedsIfaceGraph (Map.elems usesIn)
      seeds1 = sortOn seedKeyIfaceGraph seeds0
      (_, edgesRev) = foldl' step (Set.empty, []) seeds1
  in IntMap.fromList [ (keyIfaceEdgeIdIfaceGraph edge0.uid, edge0) | edge0 <- reverse edgesRev ]
  where
    step (used, acc) seed0 =
      let uid0 = freshIfaceEdgeIdIfaceGraph used (seedEdgeIdIfaceGraph seed0)
          edge0 = edgeFromSeedIfaceGraph units0 uid0 seed0
      in (Set.insert uid0 used, edge0 : acc)

data EdgeSeedIfaceGraph = EdgeSeedIfaceGraph
  { from :: !NodeId
  , to :: !NodeId
  , kind :: !KindDepIfaceGraph
  , uses :: !(Vector UseIfaceGraph)
  }

edgeSeedsIfaceGraph :: UseSummaryIfaceGraph -> [EdgeSeedIfaceGraph]
edgeSeedsIfaceGraph use0 =
  [ EdgeSeedIfaceGraph { from = use0.from, to = dep0.to, kind = dep0.kind, uses = dep0.uses }
  | dep0 <- V.toList use0.deps
  ]

seedKeyIfaceGraph :: EdgeSeedIfaceGraph -> (NodeId, NodeId, KindDepIfaceGraph, [UseIfaceGraph])
seedKeyIfaceGraph seed0 = (seed0.from, seed0.to, seed0.kind, V.toList seed0.uses)

seedEdgeIdIfaceGraph :: EdgeSeedIfaceGraph -> IfaceEdgeId
seedEdgeIdIfaceGraph seed0 =
  let h = hashShow64IfaceGraph (seed0.from, seed0.to, seed0.kind, V.toList seed0.uses)
  in IfaceEdgeId (unHash64IfaceGraph h)

freshIfaceEdgeIdIfaceGraph :: Set IfaceEdgeId -> IfaceEdgeId -> IfaceEdgeId
freshIfaceEdgeIdIfaceGraph used uid0
  | Set.member uid0 used = freshIfaceEdgeIdIfaceGraph used (succ uid0)
  | otherwise = uid0

edgeFromSeedIfaceGraph :: IntMap UnitIfaceGraph -> IfaceEdgeId -> EdgeSeedIfaceGraph -> EdgeIfaceGraph
edgeFromSeedIfaceGraph units0 uid0 seed0 =
  let srcMay = IntMap.lookup (keyNodeIdIfaceGraph seed0.from) units0
      dstMay = IntMap.lookup (keyNodeIdIfaceGraph seed0.to) units0
      status0 = edgeStatusIfaceGraph srcMay dstMay
      seenApi0 = maybe zeroHash64IfaceGraph (.hashApi) dstMay
      seenInline0 = maybe zeroHash64IfaceGraph (.hashInline) dstMay
  in EdgeIfaceGraph
    { uid = uid0
    , from = seed0.from
    , to = seed0.to
    , kind = seed0.kind
    , uses = seed0.uses
    , status = status0
    , seenApi = seenApi0
    , seenInline = seenInline0
    }

edgeStatusIfaceGraph :: Maybe UnitIfaceGraph -> Maybe UnitIfaceGraph -> StatusEdgeIfaceGraph
edgeStatusIfaceGraph srcMay dstMay =
  case (srcMay, dstMay) of
    (Nothing, _) -> BlockedEdgeStatusIfaceGraph
    (_, Nothing) -> BlockedEdgeStatusIfaceGraph
    (Just src0, Just dst0) ->
      case (src0.status, dst0.status) of
        (ReadyNodeStatusIfaceGraph, ReadyNodeStatusIfaceGraph) -> ReadyEdgeStatusIfaceGraph
        (FailedNodeStatusIfaceGraph, _) -> BlockedEdgeStatusIfaceGraph
        (_, FailedNodeStatusIfaceGraph) -> BlockedEdgeStatusIfaceGraph
        _ -> StaleEdgeStatusIfaceGraph


metaIfaceGraph :: Word64 -> ModGraph -> IntMap UnitIfaceGraph -> IntMap EdgeIfaceGraph -> MetaIfaceGraph
metaIfaceGraph ver modGraph0 units0 edges0 =
  let
    hashMod0 = hashShow64IfaceGraph modGraph0
    uid0 = IfaceGraphId (unHash64IfaceGraph (hashShow64IfaceGraph (ver, units0, edges0)))
  in
  MetaIfaceGraph { uid = uid0, hashModGraph = hashMod0, version = ver }


usesBaseIfaceGraph :: IfaceGraph -> ModGraph -> (Map NodeId UseSummaryIfaceGraph, Set NodeId)
usesBaseIfaceGraph prev0 modGraph0 =
  let edges0 = IntMap.elems prev0.edges
      (depsByFrom, stale0) = foldl' step (Map.empty, Set.empty) edges0
      uses0 = Map.mapWithKey mkSummary depsByFrom
  in (uses0, stale0)
  where
  step :: (Map NodeId [DepUseIfaceGraph], Set NodeId) -> EdgeIfaceGraph -> (Map NodeId [DepUseIfaceGraph], Set NodeId)
  step (depsByFrom, stale0) edge0
    | not (nodeExistsIfaceGraph modGraph0 edge0.from) = (depsByFrom, stale0)
    | not (nodeExistsIfaceGraph modGraph0 edge0.to) = (depsByFrom, Set.insert edge0.from stale0)
    | not (depExistsIfaceGraph modGraph0 edge0.from edge0.to) = (depsByFrom, Set.insert edge0.from stale0)
    | otherwise =
        let dep0 = DepUseIfaceGraph { to = edge0.to, kind = edge0.kind, uses = edge0.uses }
        in (Map.insertWith (<>) edge0.from [dep0] depsByFrom, stale0)

  mkSummary from0 deps0 = normalizeUseSummaryIfaceGraph UseSummaryIfaceGraph {
      from = from0
    , deps = V.fromList deps0
    }

unitsBaseIfaceGraph :: IfaceGraph -> ModGraph -> Set NodeId -> Map NodeId UnitIfaceGraph
unitsBaseIfaceGraph prev0 modGraph0 staleUseNodes0 =
  Map.fromList
    [ (nodeId0, unitBaseIfaceGraph prev0 modGraph0 staleUseNodes0 nodeId0)
    | nodeId0 <- nodeIdsIfaceGraph modGraph0
    ]

unitBaseIfaceGraph :: IfaceGraph -> ModGraph -> Set NodeId -> NodeId -> UnitIfaceGraph
unitBaseIfaceGraph prev0 modGraph0 staleUseNodes0 nodeId0 =
  case IntMap.lookup (keyNodeIdIfaceGraph nodeId0) prev0.units of
    Nothing -> staleUnitIfaceGraph modGraph0 nodeId0
    Just unit0 ->
      let scc0 = sccNodeIfaceGraph modGraph0 nodeId0
          status0 =
            if Set.member nodeId0 staleUseNodes0 && unit0.status == ReadyNodeStatusIfaceGraph
              then StaleNodeStatusIfaceGraph
              else unit0.status
      in unit0 { scc = scc0, status = status0, exports = normalizeExportsIfaceGraph unit0.exports }

deltasUpdIfaceGraph :: IfaceGraph -> IfaceGraph -> Map NodeId UnitIfaceGraph -> Vector DeltaIfaceGraph
deltasUpdIfaceGraph prev0 graph0 unitsUpd0 =
  let deltas0 = mapMaybe deltaOne (Map.keys unitsUpd0)
      deltas1 = sortOn (\delta0 -> sortKeyNodeIfaceGraph graph0 delta0.node) deltas0
  in V.fromList deltas1
  where
    deltaOne nodeId0 = do
      prevUnit0 <- IntMap.lookup (keyNodeIdIfaceGraph nodeId0) prev0.units
      newUnit0 <- IntMap.lookup (keyNodeIdIfaceGraph nodeId0) graph0.units
      if prevUnit0.status /= ReadyNodeStatusIfaceGraph || newUnit0.status /= ReadyNodeStatusIfaceGraph
        then Nothing
        else
          let delta0 = diffUnitIfaceGraph prevUnit0 newUnit0
          in if meaningfulDeltaIfaceGraph delta0 then Just delta0 else Nothing

diffUnitIfaceGraph :: UnitIfaceGraph -> UnitIfaceGraph -> DeltaIfaceGraph
diffUnitIfaceGraph prev0 new0 =
  let prevMap = exportMapIfaceGraph prev0.exports
      newMap = exportMapIfaceGraph new0.exports
      keys0 = L.sort (Set.toList (Map.keysSet prevMap `Set.union` Map.keysSet newMap))
      exportsChanged0 = V.fromList (mapMaybe (changeOne prevMap newMap) keys0)
  in DeltaIfaceGraph
    { node = new0.node
    , apiChanged = prev0.hashApi /= new0.hashApi
    , namespaceChanged = prev0.hashNamespace /= new0.hashNamespace
    , inlineChanged = prev0.hashInline /= new0.hashInline
    , exportsChanged = exportsChanged0
    }
  where
    changeOne prevMap0 newMap0 key0 =
      case (Map.lookup key0 prevMap0, Map.lookup key0 newMap0) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just hash0) -> Just (AddExportIfaceGraph key0 hash0)
        (Just hash0, Nothing) -> Just (RemoveExportIfaceGraph key0 hash0)
        (Just prevHash0, Just newHash0)
          | prevHash0 == newHash0 -> Nothing
          | otherwise -> Just (UpdateExportIfaceGraph key0 prevHash0 newHash0)

planIfaceGraph :: IfaceGraph -> Vector DeltaIfaceGraph -> Set NodeId -> PlanInvalidateIfaceGraph
planIfaceGraph graph0 deltas0 refreshed0 =
  let direct0 = concatMap (marksDeltaIfaceGraph graph0) (V.toList deltas0)
      full0 = concatMap (expandMarkIfaceGraph graph0) direct0
      full1 = filter (\mark0 -> not (Set.member mark0.node refreshed0)) full0
      marks0 = normalizeMarksIfaceGraph graph0 full1
      frontier0 = frontierFromMarksIfaceGraph graph0 marks0
  in PlanInvalidateIfaceGraph { frontier = frontier0, marks = marks0 }

marksDeltaIfaceGraph :: IfaceGraph -> DeltaIfaceGraph -> [MarkInvalidateIfaceGraph]
marksDeltaIfaceGraph graph0 delta0
  | not (meaningfulDeltaIfaceGraph delta0) = []
  | otherwise =
      let edgeIds0 = V.toList (Map.findWithDefault V.empty delta0.node graph0.byTo)
      in mapMaybe (markEdgeIfaceGraph graph0 delta0) edgeIds0

markEdgeIfaceGraph :: IfaceGraph -> DeltaIfaceGraph -> IfaceEdgeId -> Maybe MarkInvalidateIfaceGraph
markEdgeIfaceGraph graph0 delta0 edgeId0 = do
  edge0 <- IntMap.lookup (keyIfaceEdgeIdIfaceGraph edgeId0) graph0.edges
  if edge0.status == BlockedEdgeStatusIfaceGraph
    then Nothing
    else
      case edge0.kind of
        ApiDepIfaceGraph ->
          if affectsUsesIfaceGraph delta0 edge0.uses
            then Just MarkInvalidateIfaceGraph
              { stage = BodyStageFrontIfaceGraph
              , node = edge0.from
              , reason = ApiReasonInvalidateIfaceGraph delta0.node edgeId0 edge0.uses
              }
            else Nothing
        ReExportDepIfaceGraph ->
          if affectsUsesIfaceGraph delta0 edge0.uses
            then Just MarkInvalidateIfaceGraph
              { stage = IfaceStageFrontIfaceGraph
              , node = edge0.from
              , reason = ReExportReasonInvalidateIfaceGraph delta0.node edgeId0 edge0.uses
              }
            else Nothing
        InlineDepIfaceGraph ->
          if delta0.inlineChanged
            then Just MarkInvalidateIfaceGraph
              { stage = LowerStageFrontIfaceGraph
              , node = edge0.from
              , reason = InlineReasonInvalidateIfaceGraph delta0.node edgeId0
              }
            else Nothing

expandMarkIfaceGraph :: IfaceGraph -> MarkInvalidateIfaceGraph -> [MarkInvalidateIfaceGraph]
expandMarkIfaceGraph graph0 mark0 =
  let sccId0 = sccNodeIfaceGraph graph0.modGraph mark0.node
      peers0 = V.toList (nodesSccIfaceGraph graph0.modGraph sccId0)
      stages0 = stageClosureIfaceGraph mark0.stage
      self0 =
        [ MarkInvalidateIfaceGraph { stage = stage0, node = mark0.node, reason = mark0.reason }
        | stage0 <- stages0
        ]
      peer0 =
        [ MarkInvalidateIfaceGraph
            { stage = stage0
            , node = nodeId0
            , reason = SccReasonInvalidateIfaceGraph sccId0
            }
        | nodeId0 <- peers0
        , nodeId0 /= mark0.node
        , stage0 <- stages0
        ]
  in self0 <> peer0

normalizeMarksIfaceGraph :: IfaceGraph -> [MarkInvalidateIfaceGraph] -> Vector MarkInvalidateIfaceGraph
normalizeMarksIfaceGraph graph0 marks0 =
  let marks1 = L.nubBy eqMarkIfaceGraph marks0
      marks2 = sortOn (sortKeyMarkIfaceGraph graph0) marks1
  in V.fromList marks2

eqMarkIfaceGraph :: MarkInvalidateIfaceGraph -> MarkInvalidateIfaceGraph -> Bool
eqMarkIfaceGraph lhs rhs =
  lhs.stage == rhs.stage
    && lhs.node == rhs.node
    && eqReasonIfaceGraph lhs.reason rhs.reason

eqReasonIfaceGraph :: ReasonInvalidateIfaceGraph -> ReasonInvalidateIfaceGraph -> Bool
eqReasonIfaceGraph lhs rhs =
  case (lhs, rhs) of
    (ApiReasonInvalidateIfaceGraph nodeA edgeA usesA, ApiReasonInvalidateIfaceGraph nodeB edgeB usesB) ->
      nodeA == nodeB && edgeA == edgeB && usesA == usesB
    (ReExportReasonInvalidateIfaceGraph nodeA edgeA usesA, ReExportReasonInvalidateIfaceGraph nodeB edgeB usesB) ->
      nodeA == nodeB && edgeA == edgeB && usesA == usesB
    (InlineReasonInvalidateIfaceGraph nodeA edgeA, InlineReasonInvalidateIfaceGraph nodeB edgeB) ->
      nodeA == nodeB && edgeA == edgeB
    (SccReasonInvalidateIfaceGraph sccA, SccReasonInvalidateIfaceGraph sccB) ->
      sccA == sccB
    _ -> False

frontierFromMarksIfaceGraph :: IfaceGraph -> Vector MarkInvalidateIfaceGraph -> FrontierIfaceGraph
frontierFromMarksIfaceGraph graph0 marks0 =
  let body0 = sortNodesIfaceGraph graph0 (nodesStageIfaceGraph BodyStageFrontIfaceGraph marks0)
      iface0 = sortNodesIfaceGraph graph0 (nodesStageIfaceGraph IfaceStageFrontIfaceGraph marks0)
      lower0 = sortNodesIfaceGraph graph0 (nodesStageIfaceGraph LowerStageFrontIfaceGraph marks0)
      emit0 = sortNodesIfaceGraph graph0 (nodesStageIfaceGraph EmitStageFrontIfaceGraph marks0)
  in FrontierIfaceGraph
    { body = body0
    , iface = iface0
    , lower = lower0
    , emit = emit0
    }

nodesStageIfaceGraph :: StageFrontIfaceGraph -> Vector MarkInvalidateIfaceGraph -> Vector NodeId
nodesStageIfaceGraph stage0 marks0 =
  let nodes0 =
        [ mark0.node
        | mark0 <- V.toList marks0
        , mark0.stage == stage0
        ]
  in V.fromList (L.nub nodes0)

refreshedNodesIfaceGraph :: Map NodeId UnitIfaceGraph -> Map NodeId UseSummaryIfaceGraph -> Set NodeId
refreshedNodesIfaceGraph unitsUpd0 usesUpd0 =
  Set.fromList (Map.keys unitsUpd0 <> Map.keys usesUpd0)

meaningfulDeltaIfaceGraph :: DeltaIfaceGraph -> Bool
meaningfulDeltaIfaceGraph delta0 =
  delta0.apiChanged
    || delta0.namespaceChanged
    || delta0.inlineChanged
    || not (V.null delta0.exportsChanged)

affectsUsesIfaceGraph :: DeltaIfaceGraph -> Vector UseIfaceGraph -> Bool
affectsUsesIfaceGraph delta0 uses0 = any (affectsUseIfaceGraph delta0) (V.toList uses0)

affectsUseIfaceGraph :: DeltaIfaceGraph -> UseIfaceGraph -> Bool
affectsUseIfaceGraph delta0 use0 =
  case use0 of
    AllUseIfaceGraph -> delta0.apiChanged
    NamespaceUseIfaceGraph -> delta0.namespaceChanged
    ValueUseIfaceGraph name0 -> Set.member (ValueKeyIfaceGraph name0) keys0
    TypeUseIfaceGraph name0 -> Set.member (TypeKeyIfaceGraph name0) keys0
    CtorUseIfaceGraph name0 -> Set.member (CtorKeyIfaceGraph name0) keys0
    AliasUseIfaceGraph name0 -> Set.member (AliasKeyIfaceGraph name0) keys0
    EffectUseIfaceGraph name0 -> Set.member (EffectKeyIfaceGraph name0) keys0
    ForeignUseIfaceGraph name0 -> Set.member (ForeignKeyIfaceGraph name0) keys0
  where
    keys0 = Set.fromList (map changeKeyIfaceGraph (V.toList delta0.exportsChanged))

changeKeyIfaceGraph :: ChangeExportIfaceGraph -> ExportKeyIfaceGraph
changeKeyIfaceGraph change0 =
  case change0 of
    AddExportIfaceGraph key0 _ -> key0
    RemoveExportIfaceGraph key0 _ -> key0
    UpdateExportIfaceGraph key0 _ _ -> key0

normalizeUnitIfaceGraph :: ModGraph -> UnitIfaceGraph -> UnitIfaceGraph
normalizeUnitIfaceGraph modGraph0 unit0 =
  let scc0 = sccNodeIfaceGraph modGraph0 unit0.node
      exports0 = normalizeExportsIfaceGraph unit0.exports
  in unit0 { scc = scc0, exports = exports0 }

normalizeExportsIfaceGraph :: Vector ExportIfaceGraph -> Vector ExportIfaceGraph
normalizeExportsIfaceGraph exports0 =
  let exports1 = sortOn exportSortKeyIfaceGraph (V.toList exports0)
      exports2 = Map.elems (Map.fromListWith keepLeftIfaceGraph [(exportKeyIfaceGraph export0, export0) | export0 <- exports1])
  in V.fromList (sortOn exportSortKeyIfaceGraph exports2)

normalizeUseSummaryIfaceGraph :: UseSummaryIfaceGraph -> UseSummaryIfaceGraph
normalizeUseSummaryIfaceGraph use0 =
  let depMap0 =
        Map.fromListWith mergeUsesIfaceGraph
          [ ((dep0.to, dep0.kind), normalizeUsesIfaceGraph dep0.uses)
          | dep0 <- V.toList use0.deps
          ]
      deps0 =
        [ DepUseIfaceGraph { to = to0, kind = kind0, uses = uses0 }
        | ((to0, kind0), uses0) <- Map.toList depMap0
        ]
      deps1 = sortOn depSortKeyIfaceGraph deps0
  in use0 { deps = V.fromList deps1 }

normalizeUsesIfaceGraph :: Vector UseIfaceGraph -> Vector UseIfaceGraph
normalizeUsesIfaceGraph uses0 =
  let uses1 = L.nub (sortOn useSortKeyIfaceGraph (V.toList uses0))
      hasAll0 = AllUseIfaceGraph `elem` uses1
      hasNs0 = NamespaceUseIfaceGraph `elem` uses1
      uses2
        | hasAll0 && hasNs0 = [NamespaceUseIfaceGraph, AllUseIfaceGraph]
        | hasAll0 = [AllUseIfaceGraph]
        | otherwise = uses1
  in V.fromList uses2

mergeUsesIfaceGraph :: Vector UseIfaceGraph -> Vector UseIfaceGraph -> Vector UseIfaceGraph
mergeUsesIfaceGraph lhs rhs = normalizeUsesIfaceGraph (lhs <> rhs)

keepLeftIfaceGraph :: a -> a -> a
keepLeftIfaceGraph lhs _ = lhs

validateMetaIfaceGraph :: IfaceGraph -> [Diag]
validateMetaIfaceGraph graph0 =
  let wantHash0 = hashShow64IfaceGraph graph0.modGraph
  in if graph0.meta.hashModGraph == wantHash0
    then []
    else [diagIfaceGraph "ifacegraph.meta.hashModGraph" "hashModGraph does not match current module graph hash"]

validateUnitsIfaceGraph :: IfaceGraph -> [Diag]
validateUnitsIfaceGraph graph0 =
  let nodeIds0 = nodeIdsIfaceGraph graph0.modGraph
      missing0 =
        [ diagIfaceGraph "ifacegraph.units.missing" ("missing interface unit for node " <> showTextIfaceGraph nodeId0)
        | nodeId0 <- nodeIds0
        , isNothing (IntMap.lookup (keyNodeIdIfaceGraph nodeId0) graph0.units)
        ]
      bad0 = concatMap (validateUnitIfaceGraph graph0) (IntMap.elems graph0.units)
  in missing0 <> bad0

validateUnitIfaceGraph :: IfaceGraph -> UnitIfaceGraph -> [Diag]
validateUnitIfaceGraph graph0 unit0 =
  let nodeMay = IntMap.lookup (keyNodeIdIfaceGraph unit0.node) graph0.modGraph.nodes
      diagsNode =
        if isNothing nodeMay
          then [diagIfaceGraph "ifacegraph.unit.node" ("unit references missing module node " <> showTextIfaceGraph unit0.node)]
          else []
      diagsScc =
        case nodeMay of
          Nothing -> []
          Just node0 ->
            if unit0.scc == node0.scc
              then []
              else [diagIfaceGraph "ifacegraph.unit.scc" ("unit SCC does not match module graph for node " <> showTextIfaceGraph unit0.node)]
      diagsExports =
        if unit0.exports == normalizeExportsIfaceGraph unit0.exports
          then []
          else [diagIfaceGraph "ifacegraph.unit.exports" ("exports are not normalized for node " <> showTextIfaceGraph unit0.node)]
  in diagsNode <> diagsScc <> diagsExports

validateEdgesIfaceGraph :: IfaceGraph -> [Diag]
validateEdgesIfaceGraph graph0 = concatMap (validateEdgeIfaceGraph graph0) (IntMap.elems graph0.edges)

validateEdgeIfaceGraph :: IfaceGraph -> EdgeIfaceGraph -> [Diag]
validateEdgeIfaceGraph graph0 edge0 =
  let srcMay = IntMap.lookup (keyNodeIdIfaceGraph edge0.from) graph0.units
      dstMay = IntMap.lookup (keyNodeIdIfaceGraph edge0.to) graph0.units
      diagsNode =
        [ diagIfaceGraph "ifacegraph.edge.from" ("edge references missing source node " <> showTextIfaceGraph edge0.from)
        | isNothing srcMay
        ]
          <> [ diagIfaceGraph "ifacegraph.edge.to" ("edge references missing target node " <> showTextIfaceGraph edge0.to)
             | isNothing dstMay
             ]
      diagsUses =
        if edge0.uses == normalizeUsesIfaceGraph edge0.uses
          then []
          else [diagIfaceGraph "ifacegraph.edge.uses" ("edge uses are not normalized for edge " <> showTextIfaceGraph edge0.uid)]
      diagsStatus =
        case (srcMay, dstMay, edge0.status) of
          (Just src0, Just dst0, ReadyEdgeStatusIfaceGraph)
            | src0.status == ReadyNodeStatusIfaceGraph && dst0.status == ReadyNodeStatusIfaceGraph -> []
            | otherwise -> [diagIfaceGraph "ifacegraph.edge.status.ready" ("ready edge has non-ready endpoint " <> showTextIfaceGraph edge0.uid)]
          (Just src0, Just dst0, StaleEdgeStatusIfaceGraph)
            | src0.status == FailedNodeStatusIfaceGraph || dst0.status == FailedNodeStatusIfaceGraph ->
                [diagIfaceGraph "ifacegraph.edge.status.stale" ("stale edge should be blocked " <> showTextIfaceGraph edge0.uid)]
            | src0.status == ReadyNodeStatusIfaceGraph && dst0.status == ReadyNodeStatusIfaceGraph ->
                [diagIfaceGraph "ifacegraph.edge.status.stale" ("stale edge has ready endpoints " <> showTextIfaceGraph edge0.uid)]
            | otherwise -> []
          (Just src0, Just dst0, BlockedEdgeStatusIfaceGraph)
            | src0.status == ReadyNodeStatusIfaceGraph && dst0.status == ReadyNodeStatusIfaceGraph ->
                [diagIfaceGraph "ifacegraph.edge.status.blocked" ("blocked edge has ready endpoints " <> showTextIfaceGraph edge0.uid)]
            | otherwise -> []
          _ -> []
      diagsSeen =
        case dstMay of
          Nothing -> []
          Just dst0 ->
            if edge0.seenApi == dst0.hashApi && edge0.seenInline == dst0.hashInline
              then []
              else [diagIfaceGraph "ifacegraph.edge.seen" ("seen hashes do not match target unit for edge " <> showTextIfaceGraph edge0.uid)]
  in diagsNode <> diagsUses <> diagsStatus <> diagsSeen


validateIndexesIfaceGraph :: IfaceGraph -> [Diag]
validateIndexesIfaceGraph graph0 =
  let
    byFrom0 = indexByFromIfaceGraph graph0.edges
    byTo0 = indexByToIfaceGraph graph0.edges
    diagFrom = if graph0.byFrom == byFrom0 then
        []
      else
        [diagIfaceGraph "ifacegraph.index.byFrom" "byFrom index is inconsistent with edges"]
    diagTo = if graph0.byTo == byTo0 then
        []
      else
        [diagIfaceGraph "ifacegraph.index.byTo" "byTo index is inconsistent with edges"]
  in
  diagFrom <> diagTo

indexByFromIfaceGraph :: IntMap EdgeIfaceGraph -> Map NodeId (Vector IfaceEdgeId)
indexByFromIfaceGraph edges0 =
  Map.map finalize (foldl' step Map.empty (IntMap.elems edges0))
  where
  step :: Map NodeId [IfaceEdgeId] -> EdgeIfaceGraph -> Map NodeId [IfaceEdgeId]
  step acc edge0 = Map.insertWith (<>) edge0.from [edge0.uid] acc
  finalize edgeIds0 = V.fromList (sortOn keyIfaceEdgeIdIfaceGraph edgeIds0)


indexByToIfaceGraph :: IntMap EdgeIfaceGraph -> Map NodeId (Vector IfaceEdgeId)
indexByToIfaceGraph edges0 =
  Map.map finalize (foldl' step Map.empty (IntMap.elems edges0))
  where
  step :: Map NodeId [IfaceEdgeId] -> EdgeIfaceGraph -> Map NodeId [IfaceEdgeId]
  step acc edge0 = Map.insertWith (<>) edge0.to [edge0.uid] acc
  finalize edgeIds0 = V.fromList (sortOn keyIfaceEdgeIdIfaceGraph edgeIds0)


staleUnitIfaceGraph :: ModGraph -> NodeId -> UnitIfaceGraph
staleUnitIfaceGraph modGraph0 nodeId0 =
  UnitIfaceGraph
    { node = nodeId0
    , scc = sccNodeIfaceGraph modGraph0 nodeId0
    , status = StaleNodeStatusIfaceGraph
    , hashApi = zeroHash64IfaceGraph
    , hashNamespace = zeroHash64IfaceGraph
    , hashInline = zeroHash64IfaceGraph
    , exports = V.empty
    }

nodeExistsIfaceGraph :: ModGraph -> NodeId -> Bool
nodeExistsIfaceGraph modGraph0 nodeId0 = IntMap.member (keyNodeIdIfaceGraph nodeId0) modGraph0.nodes

depExistsIfaceGraph :: ModGraph -> NodeId -> NodeId -> Bool
depExistsIfaceGraph modGraph0 from0 to0 =
  case IntMap.lookup (keyNodeIdIfaceGraph from0) modGraph0.nodes of
    Nothing -> False
    Just node0 ->
      any (edgeTargetsIfaceGraph modGraph0 to0) (V.toList node0.imports)

edgeTargetsIfaceGraph :: ModGraph -> NodeId -> EdgeId -> Bool
edgeTargetsIfaceGraph modGraph0 to0 edgeId0 =
  case IntMap.lookup (keyEdgeIdIfaceGraph edgeId0) modGraph0.edges of
    Nothing -> False
    Just edge0 -> edge0.toMay == Just to0

nodeIdsIfaceGraph :: ModGraph -> [NodeId]
nodeIdsIfaceGraph modGraph0 = map (.uid) (IntMap.elems modGraph0.nodes)

sccNodeIfaceGraph :: ModGraph -> NodeId -> SccId
sccNodeIfaceGraph modGraph0 nodeId0 =
  case IntMap.lookup (keyNodeIdIfaceGraph nodeId0) modGraph0.nodes of
    Just node0 -> node0.scc
    Nothing -> SccId 0

nodesSccIfaceGraph :: ModGraph -> SccId -> Vector NodeId
nodesSccIfaceGraph modGraph0 sccId0 =
  case IntMap.lookup (keySccIdIfaceGraph sccId0) modGraph0.sccs of
    Just scc0 -> scc0.nodes
    Nothing -> V.empty

sortNodesIfaceGraph :: IfaceGraph -> Vector NodeId -> Vector NodeId
sortNodesIfaceGraph graph0 nodes0 =
  V.fromList (sortOn (sortKeyNodeIfaceGraph graph0) (L.nub (V.toList nodes0)))

sortKeyNodeIfaceGraph :: IfaceGraph -> NodeId -> (Int, Text, Word64)
sortKeyNodeIfaceGraph graph0 nodeId0 =
  case IntMap.lookup (keyNodeIdIfaceGraph nodeId0) graph0.modGraph.nodes of
    Nothing -> (maxBound, "", wordNodeIdIfaceGraph nodeId0)
    Just node0 ->
      let topo0 =
            case IntMap.lookup (keySccIdIfaceGraph node0.scc) graph0.modGraph.sccs of
              Nothing -> maxBound
              Just scc0 -> scc0.topoIx
      in (topo0, showTextIfaceGraph node0.name, wordNodeIdIfaceGraph nodeId0)

sortKeyMarkIfaceGraph :: IfaceGraph -> MarkInvalidateIfaceGraph -> (Int, (Int, Text, Word64), (Int, Word64, Word64, [UseIfaceGraph]))
sortKeyMarkIfaceGraph graph0 mark0 =
  ( fromEnum mark0.stage
  , sortKeyNodeIfaceGraph graph0 mark0.node
  , sortKeyReasonIfaceGraph mark0.reason
  )

sortKeyReasonIfaceGraph :: ReasonInvalidateIfaceGraph -> (Int, Word64, Word64, [UseIfaceGraph])
sortKeyReasonIfaceGraph reason0 =
  case reason0 of
    ApiReasonInvalidateIfaceGraph node0 edge0 uses0 ->
      (0, wordNodeIdIfaceGraph node0, wordIfaceEdgeIdIfaceGraph edge0, V.toList uses0)
    ReExportReasonInvalidateIfaceGraph node0 edge0 uses0 ->
      (1, wordNodeIdIfaceGraph node0, wordIfaceEdgeIdIfaceGraph edge0, V.toList uses0)
    InlineReasonInvalidateIfaceGraph node0 edge0 ->
      (2, wordNodeIdIfaceGraph node0, wordIfaceEdgeIdIfaceGraph edge0, [])
    SccReasonInvalidateIfaceGraph scc0 ->
      (3, wordSccIdIfaceGraph scc0, 0, [])

stageClosureIfaceGraph :: StageFrontIfaceGraph -> [StageFrontIfaceGraph]
stageClosureIfaceGraph stage0 =
  case stage0 of
    BodyStageFrontIfaceGraph ->
      [ BodyStageFrontIfaceGraph
      , IfaceStageFrontIfaceGraph
      , LowerStageFrontIfaceGraph
      , EmitStageFrontIfaceGraph
      ]
    IfaceStageFrontIfaceGraph ->
      [ IfaceStageFrontIfaceGraph
      , LowerStageFrontIfaceGraph
      , EmitStageFrontIfaceGraph
      ]
    LowerStageFrontIfaceGraph ->
      [ LowerStageFrontIfaceGraph
      , EmitStageFrontIfaceGraph
      ]
    EmitStageFrontIfaceGraph ->
      [ EmitStageFrontIfaceGraph ]

exportMapIfaceGraph :: Vector ExportIfaceGraph -> Map ExportKeyIfaceGraph Hash64
exportMapIfaceGraph exports0 =
  Map.fromList [(exportKeyIfaceGraph export0, exportHashIfaceGraph export0) | export0 <- V.toList exports0]

exportKeyIfaceGraph :: ExportIfaceGraph -> ExportKeyIfaceGraph
exportKeyIfaceGraph export0 =
  case export0 of
    ValueExportIfaceGraph name0 _ -> ValueKeyIfaceGraph name0
    TypeExportIfaceGraph name0 _ -> TypeKeyIfaceGraph name0
    CtorExportIfaceGraph name0 _ -> CtorKeyIfaceGraph name0
    AliasExportIfaceGraph name0 _ -> AliasKeyIfaceGraph name0
    EffectExportIfaceGraph name0 _ -> EffectKeyIfaceGraph name0
    ForeignExportIfaceGraph name0 _ -> ForeignKeyIfaceGraph name0

exportHashIfaceGraph :: ExportIfaceGraph -> Hash64
exportHashIfaceGraph export0 =
  case export0 of
    ValueExportIfaceGraph _ hash0 -> hash0
    TypeExportIfaceGraph _ hash0 -> hash0
    CtorExportIfaceGraph _ hash0 -> hash0
    AliasExportIfaceGraph _ hash0 -> hash0
    EffectExportIfaceGraph _ hash0 -> hash0
    ForeignExportIfaceGraph _ hash0 -> hash0

exportSortKeyIfaceGraph :: ExportIfaceGraph -> (ExportKeyIfaceGraph, Hash64)
exportSortKeyIfaceGraph export0 = (exportKeyIfaceGraph export0, exportHashIfaceGraph export0)

depSortKeyIfaceGraph :: DepUseIfaceGraph -> (NodeId, KindDepIfaceGraph, [UseIfaceGraph])
depSortKeyIfaceGraph dep0 = (dep0.to, dep0.kind, V.toList dep0.uses)

useSortKeyIfaceGraph :: UseIfaceGraph -> (Int, Text)
useSortKeyIfaceGraph use0 =
  case use0 of
    NamespaceUseIfaceGraph -> (0, "")
    AllUseIfaceGraph -> (1, "")
    ValueUseIfaceGraph name0 -> (2, name0)
    TypeUseIfaceGraph name0 -> (3, name0)
    CtorUseIfaceGraph name0 -> (4, name0)
    AliasUseIfaceGraph name0 -> (5, name0)
    EffectUseIfaceGraph name0 -> (6, name0)
    ForeignUseIfaceGraph name0 -> (7, name0)

diagIfaceGraph :: Text -> Text -> Diag
diagIfaceGraph code0 = mkDiag (CodeDiag code0) IfaceGraphDG ErrorDS emptyRange

showTextIfaceGraph :: Show a => a -> Text
showTextIfaceGraph = Text.pack . show

zeroHash64IfaceGraph :: Hash64
zeroHash64IfaceGraph = Hash64 0

hashShow64IfaceGraph :: Show a => a -> Hash64
hashShow64IfaceGraph value0 = hashString64IfaceGraph (show value0)

hashString64IfaceGraph :: String -> Hash64
hashString64IfaceGraph chars0 =
  let start0 = 14695981039346656037
      prime0 = 1099511628211
      step acc ch = (acc `xor` fromIntegral (fromEnum ch)) * prime0
  in Hash64 (foldl' step start0 chars0)

unHash64IfaceGraph :: Hash64 -> Word64
unHash64IfaceGraph (Hash64 word0) = word0

keyNodeIdIfaceGraph :: NodeId -> Int
keyNodeIdIfaceGraph (NodeId word0) = fromIntegral word0

keyEdgeIdIfaceGraph :: EdgeId -> Int
keyEdgeIdIfaceGraph (EdgeId word0) = fromIntegral word0

keyIfaceEdgeIdIfaceGraph :: IfaceEdgeId -> Int
keyIfaceEdgeIdIfaceGraph (IfaceEdgeId word0) = fromIntegral word0

keySccIdIfaceGraph :: SccId -> Int
keySccIdIfaceGraph (SccId word0) = fromIntegral word0

wordNodeIdIfaceGraph :: NodeId -> Word64
wordNodeIdIfaceGraph (NodeId word0) = word0

wordIfaceEdgeIdIfaceGraph :: IfaceEdgeId -> Word64
wordIfaceEdgeIdIfaceGraph (IfaceEdgeId word0) = word0

wordSccIdIfaceGraph :: SccId -> Word64
wordSccIdIfaceGraph (SccId word0) = word0