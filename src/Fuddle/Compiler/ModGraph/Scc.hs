{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.ModGraph.Scc
  ( ResSccMod(..)
  , buildSccsMod
  ) where

import Control.Monad (unless, when)
import Control.Monad.State.Strict (State, execState, get, gets, modify', put)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Vector as V

import Fuddle.Compiler.ModGraph.Name (ModName)
import Fuddle.Compiler.ModGraph.Origin (OriginMod(..), PkgRefMod(..), RootKindMod(..), SourceLocMod(..))
import Fuddle.Compiler.ModGraph.Types
  ( EdgeId(..), NodeId(..), SccId(..), StatusEdgeModGraph(..) )
import Fuddle.Compiler.ModGraph.GraphTypes (EdgeModGraph(..), NodeModGraph(..), SccModGraph (..), KindDepModGraph (..))


data ResSccMod = ResSccMod
  { sccs :: !(Vector SccModGraph)
  , nodeToScc :: !(IntMap SccId)
  }
  deriving stock (Eq, Show)

buildSccsMod :: IntMap NodeModGraph -> IntMap EdgeModGraph -> ResSccMod
buildSccsMod nodesMod edgesMod =
  let
    keyNodeMap = IntMap.map keyNodeScc nodesMod
    adjNodeMap = buildAdjNodeScc nodesMod edgesMod keyNodeMap
    nodeOrder = V.fromList (L.sortOn (lookupKeyNodeScc keyNodeMap) (map nodeIdKeyScc (IntMap.keys nodesMod)))
    compsTarjan = buildCompsTarjanScc adjNodeMap nodeOrder
    rawsPair = buildRawsScc keyNodeMap adjNodeMap compsTarjan
    rawSccMap = fst rawsPair
    topoRaw = topoRawScc rawSccMap
    rawToSccMap = IntMap.fromList (zip (V.toList topoRaw) (map (SccId . fromIntegral) [0 ..]))
    sccVec = V.imap (mkSccTopoScc rawSccMap rawToSccMap) topoRaw
    nodeToSccMap = buildNodeToSccScc rawSccMap rawToSccMap
  in
  ResSccMod { sccs = sccVec, nodeToScc = nodeToSccMap }

data KeyNodeScc = KeyNodeScc
  { modNameKeyScc :: !ModName
  , rootKindKeyScc :: !RootKindMod
  , pkgNameKeyScc :: !Text
  , pkgVersionKeyScc :: !Text
  , locKeyScc :: !SourceLocMod
  }
  deriving stock (Eq, Ord, Show)

data RawScc = RawScc
  { keyRawScc :: !KeyNodeScc
  , nodesRawScc :: !(Vector NodeId)
  , depsRawScc :: !IntSet
  , cyclicRawScc :: !Bool
  }
  deriving stock (Eq, Show)

data ReadyRawScc = ReadyRawScc
  { keyReadyRawScc :: !KeyNodeScc
  , rawIxReadyRawScc :: !Int
  }
  deriving stock (Eq, Show)

instance Ord ReadyRawScc where
  compare lhs rhs = compare lhs.keyReadyRawScc rhs.keyReadyRawScc <> compare lhs.rawIxReadyRawScc rhs.rawIxReadyRawScc

data SlotTarjanScc = SlotTarjanScc
  { indexSlotTarjanScc :: !Int
  , lowSlotTarjanScc :: !Int
  }
  deriving stock (Eq, Show)

data StateTarjanScc = StateTarjanScc
  { nextIxTarjanScc :: !Int
  , stackTarjanScc :: ![NodeId]
  , onStackTarjanScc :: !IntSet
  , slotsTarjanScc :: !(IntMap SlotTarjanScc)
  , compsTarjanScc :: ![[NodeId]]
  }
  deriving stock (Eq, Show)

keyNodeScc :: NodeModGraph -> KeyNodeScc
keyNodeScc nodeMod =
  let
    originMod = nodeMod.origin
    pkgMod = originMod.pkg
  in
  KeyNodeScc
    { modNameKeyScc = nodeMod.name
    , rootKindKeyScc = originMod.rootKind
    , pkgNameKeyScc = pkgMod.name
    , pkgVersionKeyScc = pkgMod.version
    , locKeyScc = originMod.loc
    }

lookupKeyNodeScc :: IntMap KeyNodeScc -> NodeId -> KeyNodeScc
lookupKeyNodeScc keyNodeMap nodeId =
  case IntMap.lookup (keyScc nodeId) keyNodeMap of
    Just keyNode -> keyNode
    Nothing -> error (errScc "lookupKeyNodeScc" ("missing canonical key for node " <> show nodeId))

buildAdjNodeScc :: IntMap NodeModGraph -> IntMap EdgeModGraph -> IntMap KeyNodeScc -> IntMap (Vector NodeId)
buildAdjNodeScc nodesMod edgesMod keyNodeMap =
  let
    setAdj =
      IntMap.foldl'
        (\acc edgeMod ->
          case targetEdgeScc nodesMod edgeMod of
            Nothing -> acc
            Just toNode ->
              let
                fromKey = keyScc edgeMod.from
                toKey = keyScc toNode
              in
              IntMap.insertWith IntSet.union fromKey (IntSet.singleton toKey) acc
        )
        IntMap.empty
        edgesMod
  in
  IntMap.map
    (\targetsSet ->
      V.fromList
        (L.sortOn (lookupKeyNodeScc keyNodeMap) (map nodeIdKeyScc (IntSet.toList targetsSet)))
    )
    setAdj

targetEdgeScc :: IntMap NodeModGraph -> EdgeModGraph -> Maybe NodeId
targetEdgeScc nodesMod edgeMod
  | edgeMod.kind /= SourceDepModGraph = Nothing
  | not (statusEdgeScc edgeMod.status) = Nothing
  | not (IntMap.member (keyScc edgeMod.from) nodesMod) = Nothing
  | otherwise =
      case edgeMod.toMay of
        Just toNode | IntMap.member (keyScc toNode) nodesMod -> Just toNode
        _ -> Nothing

statusEdgeScc :: StatusEdgeModGraph -> Bool
statusEdgeScc statusEdge =
  case statusEdge of
    ReadyEdgeStatusModGraph -> True
    SelfEdgeStatusModGraph -> True
    MissingEdgeStatusModGraph -> False
    AmbiguousEdgeStatusModGraph -> False
    HiddenEdgeStatusModGraph -> False

buildCompsTarjanScc :: IntMap (Vector NodeId) -> Vector NodeId -> [[NodeId]]
buildCompsTarjanScc adjNodeMap nodeOrder =
  let
    initState =
      StateTarjanScc
        { nextIxTarjanScc = 0
        , stackTarjanScc = []
        , onStackTarjanScc = IntSet.empty
        , slotsTarjanScc = IntMap.empty
        , compsTarjanScc = []
        }
    endState =
      execState
        (mapM_ (visitRootTarjanScc adjNodeMap) (V.toList nodeOrder))
        initState
  in
  reverse endState.compsTarjanScc

visitRootTarjanScc :: IntMap (Vector NodeId) -> NodeId -> State StateTarjanScc ()
visitRootTarjanScc adjNodeMap nodeId = do
  seen =<< gets (.slotsTarjanScc)
  where
    seen slotsMap =
      unless (IntMap.member (keyScc nodeId) slotsMap) (strongTarjanScc adjNodeMap nodeId)

strongTarjanScc :: IntMap (Vector NodeId) -> NodeId -> State StateTarjanScc ()
strongTarjanScc adjNodeMap nodeId = do
  st0 <- get
  let
    ix0 = st0.nextIxTarjanScc
    nodeKey = keyScc nodeId
    slotNode = SlotTarjanScc { indexSlotTarjanScc = ix0, lowSlotTarjanScc = ix0 }
  put
    st0
      { nextIxTarjanScc = ix0 + 1
      , stackTarjanScc = nodeId : st0.stackTarjanScc
      , onStackTarjanScc = IntSet.insert nodeKey st0.onStackTarjanScc
      , slotsTarjanScc = IntMap.insert nodeKey slotNode st0.slotsTarjanScc
      }

  mapM_ (visitEdgeTarjanScc adjNodeMap nodeId) (V.toList (targetsNodeScc adjNodeMap nodeId))

  slotNode1 <- lookupSlotTarjanScc nodeId
  when (slotNode1.lowSlotTarjanScc == slotNode1.indexSlotTarjanScc) (closeCompTarjanScc nodeId)

visitEdgeTarjanScc :: IntMap (Vector NodeId) -> NodeId -> NodeId -> State StateTarjanScc ()
visitEdgeTarjanScc adjNodeMap fromNode toNode = do
  slotsMap <- gets (.slotsTarjanScc)
  case IntMap.lookup (keyScc toNode) slotsMap of
    Nothing -> do
      strongTarjanScc adjNodeMap toNode
      lowTo <- lowSlotTarjanScc <$> lookupSlotTarjanScc toNode
      lowerLowTarjanScc fromNode lowTo
    Just slotTo -> do
      onStackSet <- gets (.onStackTarjanScc)
      when (IntSet.member (keyScc toNode) onStackSet) (lowerLowTarjanScc fromNode slotTo.indexSlotTarjanScc)

lookupSlotTarjanScc :: NodeId -> State StateTarjanScc SlotTarjanScc
lookupSlotTarjanScc nodeId = do
  slotsMap <- gets (.slotsTarjanScc)
  case IntMap.lookup (keyScc nodeId) slotsMap of
    Just slotNode -> pure slotNode
    Nothing -> error (errScc "lookupSlotTarjanScc" ("missing tarjan slot for node " <> show nodeId))

lowerLowTarjanScc :: NodeId -> Int -> State StateTarjanScc ()
lowerLowTarjanScc nodeId lowNew =
  modify'
    (\st0 ->
      let
        nodeKey = keyScc nodeId
      in
      case IntMap.lookup nodeKey st0.slotsTarjanScc of
        Just slotNode ->
          let
            slotNode1 = slotNode { lowSlotTarjanScc = min slotNode.lowSlotTarjanScc lowNew }
          in
          st0 { slotsTarjanScc = IntMap.insert nodeKey slotNode1 st0.slotsTarjanScc }
        Nothing -> error (errScc "lowerLowTarjanScc" ("missing tarjan slot for node " <> show nodeId))
    )

closeCompTarjanScc :: NodeId -> State StateTarjanScc ()
closeCompTarjanScc rootNode =
  modify'
    (\st0 ->
      let
        popRes = popCompTarjanScc rootNode st0.stackTarjanScc st0.onStackTarjanScc []
      in
      st0
        { stackTarjanScc = stackRestPopTarjanScc popRes
        , onStackTarjanScc = onStackRestPopTarjanScc popRes
        , compsTarjanScc = nodesPopTarjanScc popRes : st0.compsTarjanScc
        }
    )

data PopTarjanScc = PopTarjanScc
  { nodesPopTarjanScc :: ![NodeId]
  , stackRestPopTarjanScc :: ![NodeId]
  , onStackRestPopTarjanScc :: !IntSet
  }

popCompTarjanScc :: NodeId -> [NodeId] -> IntSet -> [NodeId] -> PopTarjanScc
popCompTarjanScc rootNode stackNodes onStackSet accNodes =
  case stackNodes of
    [] -> error (errScc "popCompTarjanScc" ("tarjan stack exhausted before closing component at node " <> show rootNode))
    nodeId : stackRest ->
      let
        onStack1 = IntSet.delete (keyScc nodeId) onStackSet
        acc1 = nodeId : accNodes
      in
      if nodeId == rootNode
        then PopTarjanScc { nodesPopTarjanScc = acc1, stackRestPopTarjanScc = stackRest, onStackRestPopTarjanScc = onStack1 }
        else popCompTarjanScc rootNode stackRest onStack1 acc1

buildRawsScc :: IntMap KeyNodeScc -> IntMap (Vector NodeId) -> [[NodeId]] -> (IntMap RawScc, IntMap Int)
buildRawsScc keyNodeMap adjNodeMap compsTarjan =
  let
    nodesByRaw =
      V.fromList
        (map (V.fromList . L.sortOn (lookupKeyNodeScc keyNodeMap)) compsTarjan)

    nodeToRawMap =
      V.ifoldl'
        (\acc rawIx nodesScc ->
          V.foldl' (\acc1 nodeId -> IntMap.insert (keyScc nodeId) rawIx acc1) acc nodesScc
        )
        IntMap.empty
        nodesByRaw

    rawSccMap =
      IntMap.fromList
        (V.toList (V.imap (mkRawEntryScc keyNodeMap adjNodeMap nodeToRawMap) nodesByRaw))
  in
  (rawSccMap, nodeToRawMap)

mkRawEntryScc :: IntMap KeyNodeScc -> IntMap (Vector NodeId) -> IntMap Int -> Int -> Vector NodeId -> (Int, RawScc)
mkRawEntryScc keyNodeMap adjNodeMap nodeToRawMap rawIx nodesScc =
  let
    keyRaw =
      case V.uncons nodesScc of
        Just (nodeHead, _) -> lookupKeyNodeScc keyNodeMap nodeHead
        Nothing -> error (errScc "mkRawEntryScc" ("empty raw component at index " <> show rawIx))

    depsRaw = depsRawNodeScc adjNodeMap nodeToRawMap rawIx nodesScc
    cyclicRaw = V.length nodesScc > 1 || selfCycleNodesScc adjNodeMap nodesScc
  in
  (rawIx, RawScc { keyRawScc = keyRaw, nodesRawScc = nodesScc, depsRawScc = depsRaw, cyclicRawScc = cyclicRaw })

depsRawNodeScc :: IntMap (Vector NodeId) -> IntMap Int -> Int -> Vector NodeId -> IntSet
depsRawNodeScc adjNodeMap nodeToRawMap rawIx nodesScc =
  V.foldl'
    (\depsAcc fromNode ->
      V.foldl'
        (\depsAcc1 toNode ->
          case IntMap.lookup (keyScc toNode) nodeToRawMap of
            Just rawTo | rawTo /= rawIx -> IntSet.insert rawTo depsAcc1
            _ -> depsAcc1
        )
        depsAcc
        (targetsNodeScc adjNodeMap fromNode)
    )
    IntSet.empty
    nodesScc

selfCycleNodesScc :: IntMap (Vector NodeId) -> Vector NodeId -> Bool
selfCycleNodesScc adjNodeMap nodesScc =
  V.any
    (\fromNode -> V.any (== fromNode) (targetsNodeScc adjNodeMap fromNode))
    nodesScc

topoRawScc :: IntMap RawScc -> Vector Int
topoRawScc rawSccMap =
  let
    depCountMap = IntMap.map (IntSet.size . (.depsRawScc)) rawSccMap
    revDepMap = revDepsRawScc rawSccMap
    ready0 = IntMap.foldlWithKey' (
          \readyAcc rawIx rawScc ->
          if IntSet.null rawScc.depsRawScc
            then Set.insert (ReadyRawScc { keyReadyRawScc = rawScc.keyRawScc, rawIxReadyRawScc = rawIx }) readyAcc
            else readyAcc
        )
        Set.empty
        rawSccMap
  in
  V.fromList (goTopoRawScc rawSccMap revDepMap ready0 depCountMap [])
  where
    goTopoRawScc :: IntMap RawScc -> IntMap IntSet -> Set.Set ReadyRawScc -> IntMap Int -> [Int] -> [Int]
    goTopoRawScc rawSccMap0 revDepMap readySet depCountMap0 accRaw =
      if Set.null readySet
        then
          if IntMap.null depCountMap0
            then reverse accRaw
            else error (errScc "topoRawScc" "internal SCC condensation graph is not acyclic")
        else
          let
            (readyMin, readyRest) = Set.deleteFindMin readySet
            rawIx = readyMin.rawIxReadyRawScc
            depCountMap1 = IntMap.delete rawIx depCountMap0
            dependents = IntMap.findWithDefault IntSet.empty rawIx revDepMap
            stepDep (readyAcc, depCountAcc) depIx =
              case IntMap.lookup depIx depCountAcc of
                Nothing -> (readyAcc, depCountAcc)
                Just depCount ->
                  let
                    depCount1 = depCount - 1
                  in
                  if depCount1 < 0
                    then error (errScc "topoRawScc" ("negative dependency count for raw SCC " <> show depIx))
                    else
                      let
                        depCountAcc1 = IntMap.insert depIx depCount1 depCountAcc
                        readyAcc1 =
                          if depCount1 == 0
                            then Set.insert (readyEntryRawScc rawSccMap0 depIx) readyAcc
                            else readyAcc
                      in
                      (readyAcc1, depCountAcc1)

            stepRes = IntSet.foldl' stepDep (readyRest, depCountMap1) dependents
          in
          goTopoRawScc rawSccMap0 revDepMap (fst stepRes) (snd stepRes) (rawIx : accRaw)

readyEntryRawScc :: IntMap RawScc -> Int -> ReadyRawScc
readyEntryRawScc rawSccMap rawIx =
  case IntMap.lookup rawIx rawSccMap of
    Just rawScc -> ReadyRawScc { keyReadyRawScc = rawScc.keyRawScc, rawIxReadyRawScc = rawIx }
    Nothing -> error (errScc "readyEntryRawScc" ("missing raw SCC " <> show rawIx))

revDepsRawScc :: IntMap RawScc -> IntMap IntSet
revDepsRawScc rawSccMap = IntMap.foldlWithKey' (
  \revAcc rawIx rawScc -> IntSet.foldl' (
      \revAcc1 depIx -> IntMap.insertWith IntSet.union depIx (IntSet.singleton rawIx) revAcc1)
        revAcc
        rawScc.depsRawScc
    )
    (IntMap.fromList (map (\rawIx -> (rawIx, IntSet.empty)) (IntMap.keys rawSccMap)))
    rawSccMap

mkSccTopoScc :: IntMap RawScc -> IntMap SccId -> Int -> Int -> SccModGraph
mkSccTopoScc rawSccMap rawToSccMap topoIx rawIx =
  case IntMap.lookup rawIx rawSccMap of
    Nothing -> error (errScc "mkSccTopoScc" ("missing raw SCC " <> show rawIx))
    Just rawScc ->
      let
        sccId = lookupSccIdRawScc rawToSccMap rawIx
        depsScc =
          V.fromList
            (L.sort (map (lookupSccIdRawScc rawToSccMap) (IntSet.toList rawScc.depsRawScc)))
      in
      SccModGraph
        { uid = sccId
        , nodes = rawScc.nodesRawScc
        , deps = depsScc
        , indegree = V.length depsScc
        , topoIx = topoIx
        , cyclic = rawScc.cyclicRawScc
        }

buildNodeToSccScc :: IntMap RawScc -> IntMap SccId -> IntMap SccId
buildNodeToSccScc rawSccMap rawToSccMap =
  IntMap.foldlWithKey'
    (\acc rawIx rawScc ->
      let
        sccId = lookupSccIdRawScc rawToSccMap rawIx
      in
      V.foldl' (\acc1 nodeId -> IntMap.insert (keyScc nodeId) sccId acc1) acc rawScc.nodesRawScc
    )
    IntMap.empty
    rawSccMap

lookupSccIdRawScc :: IntMap SccId -> Int -> SccId
lookupSccIdRawScc rawToSccMap rawIx =
  case IntMap.lookup rawIx rawToSccMap of
    Just sccId -> sccId
    Nothing -> error (errScc "lookupSccIdRawScc" ("missing final SCC id for raw SCC " <> show rawIx))

targetsNodeScc :: IntMap (Vector NodeId) -> NodeId -> Vector NodeId
targetsNodeScc adjNodeMap nodeId = IntMap.findWithDefault V.empty (keyScc nodeId) adjNodeMap

keyScc :: NodeId -> Int
keyScc (NodeId nodeWord) = fromIntegral nodeWord

nodeIdKeyScc :: Int -> NodeId
nodeIdKeyScc = NodeId . fromIntegral

errScc :: String -> String -> String
errScc fun msg = "Fuddle.Compiler.ModGraph.Scc." <> fun <> ": " <> msg