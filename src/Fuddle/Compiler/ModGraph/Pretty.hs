module Fuddle.Compiler.ModGraph.Pretty
  ( renderModGraph
  , renderNodeModGraph
  , renderSccModGraph
  , renderDiffModGraph
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Numeric (showHex)
import Fuddle.Compiler.Base.Core (Hash64(..), TextSize(..))
import Fuddle.Compiler.Base.Range (Range(..))
import Fuddle.Compiler.ModGraph
  ( EdgeId(..)
  , GraphId(..)
  , ModGraph(..)
  , NodeId(..)
  , PkgId(..)
  , RootId(..)
  , ScopeModGraph(..)
  , SccId(..)
  , StatusEdgeModGraph(..)
  , StatusNodeModGraph(..)
  )
import Fuddle.Compiler.ModGraph.GraphTypes (EdgeModGraph(..), KindDepModGraph(..), MetaGraphMod(..), NodeModGraph(..), RootModGraph(..), SccModGraph(..))
import Fuddle.Compiler.ModGraph.Diff (DiffModGraph(..))
import Fuddle.Compiler.ModGraph.Header
  ( AliasHdrMod(..)
  , ExposeHdrMod(..)
  , ImportHdrMod(..)
  )
import Fuddle.Compiler.ModGraph.Name
  ( ModName(..)
  , QualImportModName(..)
  , SegModName(..)
  )
import Fuddle.Compiler.ModGraph.Origin
  ( OriginMod(..)
  , PkgRefMod(..)
  , RootKindMod(..)
  , SourceLocMod(..)
  )
import Fuddle.Compiler.ModGraph.GraphTypes (ModGraph(..))

renderModGraph :: ModGraph -> Text
renderModGraph graph =
  let
    nodeIx = nodeIxMod graph
    rootsLs = sortOn keyRootMod (Vector.toList graph.roots)
    nodesLs = sortOn keyNodeMod (IntMap.elems graph.nodes)
    edgesLs = sortOn (keyEdgeMod nodeIx) (IntMap.elems graph.edges)
    sccsLs = sortOn keySccMod (IntMap.elems graph.sccs)
    topoLs = sortOn (\scc -> (scc.topoIx, scc.uid)) sccsLs
    dupLs =
      [ renderDupNameMod nodeIx modName ids
      | (modName, ids) <- Map.toAscList graph.byName
      , Vector.length ids > 1
      ]
    problemLs =
      catMaybes (map renderNodeProblemMod nodesLs) <>
      catMaybes (map (renderEdgeProblemMod nodeIx) edgesLs)
  in
  T.intercalate "\n"
    [ sectionText "meta"
        [ renderMetaGraphMod graph.meta
        , "counts roots=" <> showText (length rootsLs)
            <> " nodes=" <> showText (length nodesLs)
            <> " edges=" <> showText (length edgesLs)
            <> " sccs=" <> showText (length sccsLs)
        ]
    , sectionText "roots" (map renderRootModGraph rootsLs)
    , sectionText "nodes" (map renderNodeModGraph nodesLs)
    , sectionText "edges" (map (renderEdgeModGraph nodeIx) edgesLs)
    , sectionText "sccs" (map renderSccModGraph sccsLs)
    , sectionText "topo" (map (renderTopoSccMod nodeIx) topoLs)
    , sectionText "duplicate-names" dupLs
    , sectionText "problems" problemLs
    ]

renderNodeModGraph :: NodeModGraph -> Text
renderNodeModGraph node =
  "node " <> renderNodeId node.uid
    <> " " <> renderModName node.name
    <> " scope=" <> renderScopeMod node.scope
    <> " status=" <> renderStatusNodeMod node.status
    <> " scc=" <> renderSccId node.scc
    <> " src=" <> renderHash64 node.hashSource
    <> " hdr=" <> renderHash64 node.hashHeader
    <> " if=" <> renderHashMayMod node.hashIfaceMay
    <> " imports=" <> renderEdgeIdsMod node.imports
    <> " imported-by=" <> renderEdgeIdsMod node.importedBy
    <> " origin=" <> renderOriginMod node.origin

renderSccModGraph :: SccModGraph -> Text
renderSccModGraph scc =
  "scc " <> renderSccId scc.uid
    <> " topo=" <> showText scc.topoIx
    <> " indegree=" <> showText scc.indegree
    <> " cyclic=" <> renderBoolText scc.cyclic
    <> " nodes=" <> renderNodeIdsMod scc.nodes
    <> " deps=" <> renderSccIdsMod scc.deps

renderDiffModGraph :: DiffModGraph -> Text
renderDiffModGraph diff =
  let
    addNodesLs = sortOn id (Vector.toList diff.addNodes)
    delNodesLs = sortOn id (Vector.toList diff.delNodes)
    addEdgesLs = sortOn id (Vector.toList diff.addEdges)
    delEdgesLs = sortOn id (Vector.toList diff.delEdges)
    changedSccsLs = sortOn id (Vector.toList diff.changedSccs)
    changedIfacesLs = sortOn id (Vector.toList diff.changedIfaces)
  in
  T.intercalate "\n"
    [ sectionText "summary"
        [ "counts add-nodes=" <> showText (length addNodesLs)
            <> " del-nodes=" <> showText (length delNodesLs)
            <> " add-edges=" <> showText (length addEdgesLs)
            <> " del-edges=" <> showText (length delEdgesLs)
            <> " changed-sccs=" <> showText (length changedSccsLs)
            <> " changed-ifaces=" <> showText (length changedIfacesLs)
        ]
    , sectionText "add-nodes" (map renderNodeId addNodesLs)
    , sectionText "del-nodes" (map renderNodeId delNodesLs)
    , sectionText "add-edges" (map renderEdgeId addEdgesLs)
    , sectionText "del-edges" (map renderEdgeId delEdgesLs)
    , sectionText "changed-sccs" (map renderSccId changedSccsLs)
    , sectionText "changed-ifaces" (map renderNodeId changedIfacesLs)
    ]

renderMetaGraphMod :: MetaGraphMod -> Text
renderMetaGraphMod meta =
  "graph=" <> renderGraphId meta.uid
    <> " version=" <> showText meta.version
    <> " workspace=" <> renderHash64 meta.hashWorkspace

renderRootModGraph :: RootModGraph -> Text
renderRootModGraph root =
  "root " <> renderRootId root.uid
    <> " name=" <> root.name
    <> " kind=" <> renderRootKindMod root.kind
    <> " path=" <> T.pack root.path
    <> " pkg=" <> maybe "-" renderPkgRefMod root.pkgMay

renderEdgeModGraph :: Map.Map NodeId NodeModGraph -> EdgeModGraph -> Text
renderEdgeModGraph nodeIx edge =
  "edge " <> renderEdgeId edge.uid
    <> " from=" <> renderNodeRefMod nodeIx edge.from
    <> " to=" <> renderNodeMayRefMod nodeIx edge.toMay
    <> " kind=" <> renderKindDepMod edge.kind
    <> " status=" <> renderStatusEdgeMod edge.status
    <> " import=" <> renderImportHdrMod edge.importHdr

renderTopoSccMod :: Map.Map NodeId NodeModGraph -> SccModGraph -> Text
renderTopoSccMod nodeIx scc =
  let
    namesTxt =
      T.intercalate ", "
        [ renderNodeRefMod nodeIx nodeId
        | nodeId <- sortOn (keyNodeIdNameMod nodeIx) (Vector.toList scc.nodes)
        ]
  in
  "topo[" <> showText scc.topoIx <> "] "
    <> renderSccId scc.uid
    <> " cyclic=" <> renderBoolText scc.cyclic
    <> " nodes=[" <> namesTxt <> "]"
    <> " deps=" <> renderSccIdsMod scc.deps

renderDupNameMod :: Map.Map NodeId NodeModGraph -> ModName -> Vector NodeId -> Text
renderDupNameMod nodeIx modName nodeIds =
  let
    refsTxt =
      T.intercalate ", "
        [ renderNodeDupRefMod nodeIx nodeId
        | nodeId <- sortOn (keyDupNodeMod nodeIx) (Vector.toList nodeIds)
        ]
  in
  renderModName modName <> " -> [" <> refsTxt <> "]"

renderNodeProblemMod :: NodeModGraph -> Maybe Text
renderNodeProblemMod node =
  case node.status of
    ReadyNodeStatusModGraph -> Nothing
    HeaderErrNodeStatusModGraph ->
      Just ("node-header-error " <> renderNodeProblemRefMod node)
    MissingNodeStatusModGraph ->
      Just ("node-missing " <> renderNodeProblemRefMod node)
    ShadowedNodeStatusModGraph ->
      Just ("node-shadowed " <> renderNodeProblemRefMod node)

renderEdgeProblemMod :: Map.Map NodeId NodeModGraph -> EdgeModGraph -> Maybe Text
renderEdgeProblemMod nodeIx edge =
  case edge.status of
    ReadyEdgeStatusModGraph -> Nothing
    MissingEdgeStatusModGraph ->
      Just ("import-missing from=" <> renderNodeRefMod nodeIx edge.from
        <> " import=" <> renderImportHdrMod edge.importHdr)
    AmbiguousEdgeStatusModGraph ->
      Just ("import-ambiguous from=" <> renderNodeRefMod nodeIx edge.from
        <> " import=" <> renderImportHdrMod edge.importHdr)
    HiddenEdgeStatusModGraph ->
      Just ("import-hidden from=" <> renderNodeRefMod nodeIx edge.from
        <> " to=" <> renderNodeMayRefMod nodeIx edge.toMay
        <> " import=" <> renderImportHdrMod edge.importHdr)
    SelfEdgeStatusModGraph ->
      Just ("import-self from=" <> renderNodeRefMod nodeIx edge.from
        <> " import=" <> renderImportHdrMod edge.importHdr)

renderNodeProblemRefMod :: NodeModGraph -> Text
renderNodeProblemRefMod node =
  "node=" <> renderNodeId node.uid
    <> ":" <> renderModName node.name
    <> " origin=" <> renderOriginMod node.origin

renderNodeRefMod :: Map.Map NodeId NodeModGraph -> NodeId -> Text
renderNodeRefMod nodeIx nodeId =
  case Map.lookup nodeId nodeIx of
    Nothing -> renderNodeId nodeId <> ":?"
    Just node -> renderNodeId nodeId <> ":" <> renderModName node.name

renderNodeMayRefMod :: Map.Map NodeId NodeModGraph -> Maybe NodeId -> Text
renderNodeMayRefMod nodeIx nodeMay =
  maybe "-" (renderNodeRefMod nodeIx) nodeMay

renderNodeDupRefMod :: Map.Map NodeId NodeModGraph -> NodeId -> Text
renderNodeDupRefMod nodeIx nodeId =
  case Map.lookup nodeId nodeIx of
    Nothing -> renderNodeId nodeId <> "@?"
    Just node ->
      renderNodeId nodeId
        <> "@"
        <> renderSourceLocMod node.origin.loc
        <> " pkg=" <> renderPkgRefMod node.origin.pkg
        <> " scope=" <> renderScopeMod node.scope
        <> " status=" <> renderStatusNodeMod node.status

renderImportHdrMod :: ImportHdrMod -> Text
renderImportHdrMod imp =
  renderQualImportModName imp.target
    <> renderAliasMayMod imp.aliasMay
    <> renderExposeMayMod imp.exposeMay
    <> renderRangeMayMod imp.rangeMay
    <> " edge=" <> renderEdgeId imp.uid

renderAliasMayMod :: Maybe AliasHdrMod -> Text
renderAliasMayMod aliasMay =
  case aliasMay of
    Nothing -> ""
    Just alias -> " as " <> alias.name

renderExposeMayMod :: Maybe ExposeHdrMod -> Text
renderExposeMayMod exposeMay =
  case exposeMay of
    Nothing -> ""
    Just expose -> " exposing " <> renderExposeHdrMod expose

renderExposeHdrMod :: ExposeHdrMod -> Text
renderExposeHdrMod expose =
  case expose of
    OpenExposeHdrMod -> "(..)"
    ItemsExposeHdrMod items ->
      "(" <> T.intercalate ", " (Vector.toList items) <> ")"

renderRangeMayMod :: Maybe Range -> Text
renderRangeMayMod rangeMay =
  case rangeMay of
    Nothing -> ""
    Just range -> " @" <> renderRangeMod range

renderRangeMod :: Range -> Text
renderRangeMod range =
  renderTextSize range.start <> ".." <> renderTextSize range.end

renderModName :: ModName -> Text
renderModName modName =
  case modName of
    ModName segs ->
      T.intercalate "." [ segTxt | SegModName segTxt <- NonEmpty.toList segs ]

renderQualImportModName :: QualImportModName -> Text
renderQualImportModName qualName =
  case qualName.pkgMay of
    Nothing -> renderModName qualName.modName
    Just pkgName -> pkgName <> ":" <> renderModName qualName.modName

renderOriginMod :: OriginMod -> Text
renderOriginMod origin =
  "root=" <> renderRootId origin.root
    <> " kind=" <> renderRootKindMod origin.rootKind
    <> " pkg=" <> renderPkgRefMod origin.pkg
    <> " loc=" <> renderSourceLocMod origin.loc

renderSourceLocMod :: SourceLocMod -> Text
renderSourceLocMod sourceLoc =
  case sourceLoc of
    FileSourceLocMod pathTxt -> "file:" <> T.pack pathTxt
    ArchiveSourceLocMod archiveTxt pathTxt -> "archive(" <> archiveTxt <> "):" <> T.pack pathTxt
    VirtualSourceLocMod nameTxt -> "virtual:" <> nameTxt

renderPkgRefMod :: PkgRefMod -> Text
renderPkgRefMod pkg =
  pkg.name <> "@" <> pkg.version <> "#" <> renderPkgId pkg.uid

renderRootKindMod :: RootKindMod -> Text
renderRootKindMod rootKind =
  case rootKind of
    WorkspaceRootMod -> "workspace"
    PackageRootMod -> "package"
    RegistryRootMod -> "registry"
    VirtualRootMod -> "virtual"

renderScopeMod :: ScopeModGraph -> Text
renderScopeMod scope =
  case scope of
    WorkspaceScopeModGraph -> "workspace"
    DependencyScopeModGraph -> "dependency"
    HiddenScopeModGraph -> "hidden"

renderStatusNodeMod :: StatusNodeModGraph -> Text
renderStatusNodeMod status =
  case status of
    ReadyNodeStatusModGraph -> "ready"
    HeaderErrNodeStatusModGraph -> "header-err"
    MissingNodeStatusModGraph -> "missing"
    ShadowedNodeStatusModGraph -> "shadowed"

renderStatusEdgeMod :: StatusEdgeModGraph -> Text
renderStatusEdgeMod status =
  case status of
    ReadyEdgeStatusModGraph -> "ready"
    MissingEdgeStatusModGraph -> "missing"
    AmbiguousEdgeStatusModGraph -> "ambiguous"
    HiddenEdgeStatusModGraph -> "hidden"
    SelfEdgeStatusModGraph -> "self"

renderKindDepMod :: KindDepModGraph -> Text
renderKindDepMod depKind =
  case depKind of
    SourceDepModGraph -> "source"
    InterfaceDepModGraph -> "interface"
    RuntimeDepModGraph -> "runtime"

renderGraphId :: GraphId -> Text
renderGraphId (GraphId n) = showText n

renderNodeId :: NodeId -> Text
renderNodeId (NodeId n) = showText n

renderEdgeId :: EdgeId -> Text
renderEdgeId (EdgeId n) = showText n

renderSccId :: SccId -> Text
renderSccId (SccId n) = showText n

renderPkgId :: PkgId -> Text
renderPkgId (PkgId n) = showText n

renderRootId :: RootId -> Text
renderRootId (RootId n) = showText n

renderHash64 :: Hash64 -> Text
renderHash64 (Hash64 n) =
  let
    hexTxt = showHex n ""
    padLen = max 0 (16 - length hexTxt)
  in
  "0x" <> T.pack (replicate padLen '0' <> hexTxt)

renderHashMayMod :: Maybe Hash64 -> Text
renderHashMayMod hashMay = maybe "-" renderHash64 hashMay

renderTextSize :: TextSize -> Text
renderTextSize (TextSize n) = showText n

renderNodeIdsMod :: Vector NodeId -> Text
renderNodeIdsMod nodeIds =
  "[" <> T.intercalate ", " (map renderNodeId (Vector.toList nodeIds)) <> "]"

renderEdgeIdsMod :: Vector EdgeId -> Text
renderEdgeIdsMod edgeIds =
  "[" <> T.intercalate ", " (map renderEdgeId (Vector.toList edgeIds)) <> "]"

renderSccIdsMod :: Vector SccId -> Text
renderSccIdsMod sccIds =
  "[" <> T.intercalate ", " (map renderSccId (Vector.toList sccIds)) <> "]"

renderBoolText :: Bool -> Text
renderBoolText flag =
  if flag then "true" else "false"

sectionText :: Text -> [Text] -> Text
sectionText title blocks =
  if null blocks
    then title <> "\n (none)"
    else title <> "\n" <> T.intercalate "\n" (map (indentText "  ") blocks)

indentText :: Text -> Text -> Text
indentText prefix txt =
  T.intercalate "\n" (map (prefix <>) (T.lines txt))

showText :: Show a => a -> Text
showText = T.pack . show

nodeIxMod :: ModGraph -> Map.Map NodeId NodeModGraph
nodeIxMod graph =
  Map.fromList [ (node.uid, node) | node <- IntMap.elems graph.nodes ]

keyRootMod :: RootModGraph -> (Text, Text, FilePath, RootId)
keyRootMod root =
  ( root.name
  , renderRootKindMod root.kind
  , root.path
  , root.uid
  )

keyNodeMod :: NodeModGraph -> (ModName, Text, NodeId)
keyNodeMod node =
  ( node.name
  , originKeyMod node.origin
  , node.uid
  )

keyEdgeMod :: Map.Map NodeId NodeModGraph -> EdgeModGraph -> (Text, Text, KindDepModGraph, StatusEdgeModGraph, EdgeId)
keyEdgeMod nodeIx edge =
  ( keyNodeIdNameMod nodeIx edge.from
  , renderQualImportModName edge.importHdr.target
  , edge.kind
  , edge.status
  , edge.uid
  )

keySccMod :: SccModGraph -> (Int, SccId)
keySccMod scc = (scc.topoIx, scc.uid)

keyNodeIdNameMod :: Map.Map NodeId NodeModGraph -> NodeId -> Text
keyNodeIdNameMod nodeIx nodeId =
  case Map.lookup nodeId nodeIx of
    Nothing -> "~" <> renderNodeId nodeId
    Just node -> renderModName node.name <> "@" <> renderSourceLocMod node.origin.loc

keyDupNodeMod :: Map.Map NodeId NodeModGraph -> NodeId -> (Text, Text, NodeId)
keyDupNodeMod nodeIx nodeId =
  case Map.lookup nodeId nodeIx of
    Nothing -> ("~", "~", nodeId)
    Just node ->
      ( renderSourceLocMod node.origin.loc
      , renderPkgRefMod node.origin.pkg
      , nodeId
      )

originKeyMod :: OriginMod -> Text
originKeyMod origin =
  renderRootId origin.root
    <> "|"
    <> renderRootKindMod origin.rootKind
    <> "|"
    <> renderPkgRefMod origin.pkg
    <> "|"
    <> renderSourceLocMod origin.loc