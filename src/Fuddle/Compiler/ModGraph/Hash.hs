module Fuddle.Compiler.ModGraph.Hash
  ( hashNodeModGraph
  , hashEdgeModGraph
  , hashSccModGraph
  , hashModGraph
  ) where

import Data.Bits ((.&.), shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', sort)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import qualified Data.Vector as V

import Fuddle.Compiler.Base.Core (Hash64(..))
import Fuddle.Compiler.ModGraph ( 
    EdgeId(..), ModGraph(..), NodeId(..), ScopeModGraph(..), SccId(..)
  )
import Fuddle.Compiler.ModGraph.GraphTypes (
    EdgeModGraph(..), NodeModGraph(..), KindDepModGraph(..), RootModGraph(..), SccModGraph(..)
  )
import Fuddle.Compiler.ModGraph.Types (StatusNodeModGraph(..), StatusEdgeModGraph(..))
import Fuddle.Compiler.ModGraph.Header (ImportHdrMod(..))
import Fuddle.Compiler.ModGraph.Name (ModName(..), QualImportModName(..), SegModName(..))
import Fuddle.Compiler.ModGraph.Origin (OriginMod(..), PkgRefMod(..), RootKindMod(..), SourceLocMod(..))
import Fuddle.Compiler.ModGraph.GraphTypes (ModGraph(..))

-- These hashes are structural and deterministic.
-- They intentionally avoid snapshot-local ids where a stable semantic key exists,
-- so equivalent rebuilds can keep the same graph hash even if ids were reallocated.

hashNodeModGraph :: ModGraph -> NodeModGraph -> Hash64
hashNodeModGraph graph node =
  Hash64 $
    hashWordsW 1
      [ hashNodeKeyW node
      , hashScopeModGraphW node.scope
      , hashStatusNodeModGraphW node.status
      , rawHash64 node.hashSource
      , rawHash64 node.hashHeader
      , hashMaybeW 2 rawHash64 node.hashIfaceMay
      , hashVectorSortedW 3 (hashEdgeRefModGraphW graph) node.imports
      ]

hashEdgeModGraph :: ModGraph -> EdgeModGraph -> Hash64
hashEdgeModGraph graph edge =
  Hash64 $
    hashWordsW 4
      [ hashNodeRefModGraphW graph edge.from
      , hashQualImportModNameW edge.importHdr.target
      , hashMaybeW 5 (hashNodeRefModGraphW graph) edge.toMay
      , hashKindDepModGraphW edge.kind
      , hashStatusEdgeModGraphW edge.status
      ]

hashSccModGraph :: ModGraph -> SccModGraph -> Hash64
hashSccModGraph graph scc =
  Hash64 $
    hashWordsW 6
      [ hashVectorSortedW 7 (hashNodeRefModGraphW graph) scc.nodes
      , hashVectorSortedW 8 (hashSccRefModGraphW graph) scc.deps
      , fromIntegral scc.indegree
      , fromIntegral scc.topoIx
      , hashBoolW scc.cyclic
      ]

hashModGraph :: ModGraph -> Hash64
hashModGraph graph =
  Hash64 $
    hashWordsW 9
      [ hashVectorSortedW 10 hashRootModGraphW graph.roots
      , hashListSortedW 11 (rawHash64 . hashNodeModGraph graph) (IntMap.elems graph.nodes)
      , hashListSortedW 12 (rawHash64 . hashEdgeModGraph graph) (IntMap.elems graph.edges)
      , hashListSortedW 13 (rawHash64 . hashSccModGraph graph) (IntMap.elems graph.sccs)
      ]

rawHash64 :: Hash64 -> Word64
rawHash64 (Hash64 w) = w

hashBoolW :: Bool -> Word64
hashBoolW b =
  if b
    then 1
    else 0

hashWordsW :: Word64 -> [Word64] -> Word64
hashWordsW tag words0 =
  let acc0 = mixWordHashW seedHashW tag
      acc1 = mixWordHashW acc0 (fromIntegral (length words0))
      acc2 = foldl' mixWordHashW acc1 words0
  in finishHashW acc2

hashTextW :: Word64 -> Text -> Word64
hashTextW tag txt =
  let bytes = TE.encodeUtf8 txt
      acc0 = mixWordHashW seedHashW tag
      acc1 = mixWordHashW acc0 (fromIntegral (BS.length bytes))
      acc2 = BS.foldl' mixByteHashW acc1 bytes
  in finishHashW acc2

hashStringW :: Word64 -> String -> Word64
hashStringW tag = hashTextW tag . T.pack

hashMaybeW :: Word64 -> (a -> Word64) -> Maybe a -> Word64
hashMaybeW tag hashA may =
  case may of
    Nothing -> hashWordsW tag [0]
    Just a -> hashWordsW tag [1, hashA a]

hashListW :: Word64 -> (a -> Word64) -> [a] -> Word64
hashListW tag hashA xs = hashWordsW tag (map hashA xs)

hashListSortedW :: Word64 -> (a -> Word64) -> [a] -> Word64
hashListSortedW tag hashA xs = hashWordsW tag (sort (map hashA xs))

hashVectorSortedW :: Word64 -> (a -> Word64) -> V.Vector a -> Word64
hashVectorSortedW tag hashA xs = hashListSortedW tag hashA (V.toList xs)

hashSegModNameW :: SegModName -> Word64
hashSegModNameW (SegModName txt) = hashTextW 1 txt

hashModNameW :: ModName -> Word64
hashModNameW (ModName segs) = hashListW 2 hashSegModNameW (NE.toList segs)

hashQualImportModNameW :: QualImportModName -> Word64
hashQualImportModNameW qual =
  hashWordsW 3
    [ hashMaybeW 1 (hashTextW 2) qual.pkgMay
    , hashModNameW qual.modName
    ]

hashRootKindModW :: RootKindMod -> Word64
hashRootKindModW rootKind =
  case rootKind of
    WorkspaceRootMod -> 1
    PackageRootMod -> 2
    RegistryRootMod -> 3
    VirtualRootMod -> 4

hashPkgRefModW :: PkgRefMod -> Word64
hashPkgRefModW pkg =
  hashWordsW 14
    [ hashTextW 1 pkg.name
    , hashTextW 2 pkg.version
    ]

hashSourceLocModW :: SourceLocMod -> Word64
hashSourceLocModW sourceLoc =
  case sourceLoc of
    FileSourceLocMod path ->
      hashWordsW 15 [1, hashStringW 1 path]
    ArchiveSourceLocMod archive path ->
      hashWordsW 15 [2, hashTextW 2 archive, hashStringW 3 path]
    VirtualSourceLocMod label ->
      hashWordsW 15 [3, hashTextW 4 label]

hashOriginModW :: OriginMod -> Word64
hashOriginModW origin =
  hashWordsW 16
    [ hashRootKindModW origin.rootKind
    , hashPkgRefModW origin.pkg
    , hashSourceLocModW origin.loc
    ]

hashScopeModGraphW :: ScopeModGraph -> Word64
hashScopeModGraphW scope =
  case scope of
    WorkspaceScopeModGraph -> 1
    DependencyScopeModGraph -> 2
    HiddenScopeModGraph -> 3

hashStatusNodeModGraphW :: StatusNodeModGraph -> Word64
hashStatusNodeModGraphW status =
  case status of
    ReadyNodeStatusModGraph -> 1
    HeaderErrNodeStatusModGraph -> 2
    MissingNodeStatusModGraph -> 3
    ShadowedNodeStatusModGraph -> 4

hashStatusEdgeModGraphW :: StatusEdgeModGraph -> Word64
hashStatusEdgeModGraphW status =
  case status of
    ReadyEdgeStatusModGraph -> 1
    MissingEdgeStatusModGraph -> 2
    AmbiguousEdgeStatusModGraph -> 3
    HiddenEdgeStatusModGraph -> 4
    SelfEdgeStatusModGraph -> 5

hashKindDepModGraphW :: KindDepModGraph -> Word64
hashKindDepModGraphW kind =
  case kind of
    SourceDepModGraph -> 1
    InterfaceDepModGraph -> 2
    RuntimeDepModGraph -> 3

hashRootModGraphW :: RootModGraph -> Word64
hashRootModGraphW root =
  hashWordsW 17
    [ hashTextW 1 root.name
    , hashRootKindModW root.kind
    , hashStringW 2 root.path
    , hashMaybeW 3 hashPkgRefModW root.pkgMay
    ]

hashNodeKeyW :: NodeModGraph -> Word64
hashNodeKeyW node =
  hashWordsW 18
    [ hashModNameW node.name
    , hashOriginModW node.origin
    ]

hashNodeRefModGraphW :: ModGraph -> NodeId -> Word64
hashNodeRefModGraphW graph uid =
  case lookupNodeModGraphW graph uid of
    Just node -> hashNodeKeyW node
    Nothing -> hashWordsW 19 [hashNodeIdW uid]

hashEdgeRefModGraphW :: ModGraph -> EdgeId -> Word64
hashEdgeRefModGraphW graph uid =
  case lookupEdgeModGraphW graph uid of
    Just edge -> rawHash64 (hashEdgeModGraph graph edge)
    Nothing -> hashWordsW 20 [hashEdgeIdW uid]

hashSccRefModGraphW :: ModGraph -> SccId -> Word64
hashSccRefModGraphW graph uid =
  case lookupSccModGraphW graph uid of
    Just scc -> hashVectorSortedW 21 (hashNodeRefModGraphW graph) scc.nodes
    Nothing -> hashWordsW 22 [hashSccIdW uid]

hashNodeIdW :: NodeId -> Word64
hashNodeIdW (NodeId w) = w

hashEdgeIdW :: EdgeId -> Word64
hashEdgeIdW (EdgeId w) = w

hashSccIdW :: SccId -> Word64
hashSccIdW (SccId w) = w

lookupNodeModGraphW :: ModGraph -> NodeId -> Maybe NodeModGraph
lookupNodeModGraphW graph uid = IntMap.lookup (keyNodeIdW uid) graph.nodes

lookupEdgeModGraphW :: ModGraph -> EdgeId -> Maybe EdgeModGraph
lookupEdgeModGraphW graph uid = IntMap.lookup (keyEdgeIdW uid) graph.edges

lookupSccModGraphW :: ModGraph -> SccId -> Maybe SccModGraph
lookupSccModGraphW graph uid = IntMap.lookup (keySccIdW uid) graph.sccs

keyNodeIdW :: NodeId -> Int
keyNodeIdW (NodeId w) = fromIntegral w

keyEdgeIdW :: EdgeId -> Int
keyEdgeIdW (EdgeId w) = fromIntegral w

keySccIdW :: SccId -> Int
keySccIdW (SccId w) = fromIntegral w

seedHashW :: Word64
seedHashW = 0xcbf29ce484222325

primeHashW :: Word64
primeHashW = 0x100000001b3

mixByteHashW :: Word64 -> Word8 -> Word64
mixByteHashW acc byte = (acc `xor` fromIntegral byte) * primeHashW

mixWordHashW :: Word64 -> Word64 -> Word64
mixWordHashW acc word0 =
  let b0 = fromIntegral (word0 .&. 0xff)
      b1 = fromIntegral ((word0 `shiftR` 8) .&. 0xff)
      b2 = fromIntegral ((word0 `shiftR` 16) .&. 0xff)
      b3 = fromIntegral ((word0 `shiftR` 24) .&. 0xff)
      b4 = fromIntegral ((word0 `shiftR` 32) .&. 0xff)
      b5 = fromIntegral ((word0 `shiftR` 40) .&. 0xff)
      b6 = fromIntegral ((word0 `shiftR` 48) .&. 0xff)
      b7 = fromIntegral ((word0 `shiftR` 56) .&. 0xff)
  in foldl' mixByteHashW acc [b0, b1, b2, b3, b4, b5, b6, b7]

finishHashW :: Word64 -> Word64
finishHashW word0 =
  let word1 = (word0 `xor` (word0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      word2 = (word1 `xor` (word1 `shiftR` 27)) * 0x94d049bb133111eb
  in word2 `xor` (word2 `shiftR` 31)