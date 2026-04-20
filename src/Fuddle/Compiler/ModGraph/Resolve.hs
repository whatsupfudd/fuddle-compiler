{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.ModGraph.Resolve
  ( ScopeModGraph(..)
  , TargetResolveMod(..)
  , ResImportMod(..)
  , ErrResolveMod(..)
  , resolveImportsMod
  ) where

import Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Fuddle.Compiler.ModGraph.Header (HeaderMod(..), ImportHdrMod(..))
import Fuddle.Compiler.ModGraph.Name (ModName(..), QualImportModName(..))
import Fuddle.Compiler.ModGraph.Origin (OriginMod(..), PkgRefMod(..), RootKindMod(..), SourceLocMod(..))
import Fuddle.Compiler.ModGraph.Types (EdgeId(..), NodeId(..), PkgId(..), ScopeModGraph(..))


data TargetResolveMod
  = TargetNodeResolveMod !NodeId
  | MissingTargetResolveMod
  | AmbiguousTargetResolveMod !(Vector NodeId)
  | HiddenTargetResolveMod !NodeId
  deriving stock (Eq, Show)

data ResImportMod = ResImportMod
  { edge :: !EdgeId
  , from :: !NodeId
  , target :: !TargetResolveMod
  }
  deriving stock (Eq, Show)

data ErrResolveMod
  = DuplicateModuleNameErrResolveMod !ModName !(Vector OriginMod)
  | InvalidImportPkgErrResolveMod !QualImportModName
  | HiddenImportErrResolveMod !ModName
  deriving stock (Eq, Show)

data CandResolveMod = CandResolveMod
  { node :: !NodeId
  , scope :: !ScopeModGraph
  , header :: !HeaderMod
  }

resolveImportsMod
  :: Vector (NodeId, ScopeModGraph, HeaderMod)
  -> (Vector ResImportMod, Vector ErrResolveMod)
resolveImportsMod entries0 =
  let
    cands1 = sortCandsResolveMod (mkCandsResolveMod entries0)
    byName1 = byNameResolveMod cands1
    pkgNamesQual1 = pkgNamesQualResolveMod cands1
    dupErrs1 = dupErrsResolveMod cands1
    (resRev1, errRev1) = foldl' (stepNodeResolveMod pkgNamesQual1 byName1) ([], []) cands1
  in
  ( Vector.fromList (reverse resRev1)
  , dupErrs1 <> Vector.fromList (reverse errRev1)
  )

mkCandsResolveMod :: Vector (NodeId, ScopeModGraph, HeaderMod) -> [CandResolveMod]
mkCandsResolveMod entries0 =
  fmap mkCandResolveMod (Vector.toList entries0)

mkCandResolveMod :: (NodeId, ScopeModGraph, HeaderMod) -> CandResolveMod
mkCandResolveMod (node0, scope0, header0) =
  CandResolveMod { node = node0, scope = scope0, header = header0 }

sortCandsResolveMod :: [CandResolveMod] -> [CandResolveMod]
sortCandsResolveMod =
  List.sortBy (\left right -> compare (keyCandResolveMod left) (keyCandResolveMod right))

keyCandResolveMod :: CandResolveMod -> (Text, Text, ModName, SourceLocMod, NodeId)
keyCandResolveMod cand0 =
  let
    header0 = cand0.header
    origin0 = header0.origin
    pkg0 = origin0.pkg
  in
  (pkg0.name, pkg0.version, header0.name, origin0.loc, cand0.node)

byNameResolveMod :: [CandResolveMod] -> Map.Map ModName [CandResolveMod]
byNameResolveMod =
  foldl' insNameResolveMod Map.empty

insNameResolveMod :: Map.Map ModName [CandResolveMod] -> CandResolveMod -> Map.Map ModName [CandResolveMod]
insNameResolveMod acc0 cand0 =
  let
    name0 = cand0.header.name
  in
  Map.insertWith (\new0 old0 -> old0 <> new0) name0 [cand0] acc0

pkgNamesQualResolveMod :: [CandResolveMod] -> Set.Set Text
pkgNamesQualResolveMod =
  foldl' insPkgQualResolveMod Set.empty

insPkgQualResolveMod :: Set.Set Text -> CandResolveMod -> Set.Set Text
insPkgQualResolveMod acc0 cand0 =
  case cand0.header.origin.rootKind of
    VirtualRootMod -> acc0
    _ -> Set.insert cand0.header.origin.pkg.name acc0

dupErrsResolveMod :: [CandResolveMod] -> Vector ErrResolveMod
dupErrsResolveMod cands0 =
  let
    groups0 = foldl' insDupResolveMod Map.empty cands0
    errs0 = foldr stepDupResolveMod [] (Map.toAscList groups0)
  in
  Vector.fromList errs0


insDupResolveMod :: Map.Map (PkgId, ModName) [CandResolveMod] -> CandResolveMod -> Map.Map (PkgId, ModName) [CandResolveMod]
insDupResolveMod acc0 cand0 =
  let
    pkgUid0 = cand0.header.origin.pkg.uid
    name0 = cand0.header.name
  in
  Map.insertWith (\new0 old0 -> old0 <> new0) (pkgUid0, name0) [cand0] acc0


stepDupResolveMod :: ((PkgId, ModName), [CandResolveMod]) -> [ErrResolveMod] -> [ErrResolveMod]
stepDupResolveMod ((_, name0), group0) acc0 =
  case group0 of
    [] -> acc0
    [_] -> acc0
    _ ->
      let
        origins0 = Vector.fromList (fmap (\cand0 -> cand0.header.origin) group0)
      in
      DuplicateModuleNameErrResolveMod name0 origins0 : acc0

stepNodeResolveMod
  :: Set.Set Text
  -> Map.Map ModName [CandResolveMod]
  -> ([ResImportMod], [ErrResolveMod])
  -> CandResolveMod
  -> ([ResImportMod], [ErrResolveMod])
stepNodeResolveMod pkgNamesQual0 byName0 (resAcc0, errAcc0) src0 =
  foldl' (stepImportResolveMod pkgNamesQual0 byName0 src0) (resAcc0, errAcc0) (Vector.toList src0.header.imports)

stepImportResolveMod
  :: Set.Set Text
  -> Map.Map ModName [CandResolveMod]
  -> CandResolveMod
  -> ([ResImportMod], [ErrResolveMod])
  -> ImportHdrMod
  -> ([ResImportMod], [ErrResolveMod])
stepImportResolveMod pkgNamesQual0 byName0 src0 (resAcc0, errAcc0) import0 =
  let
    qual0 = import0.target
    (target0, errs0) = targetImportResolveMod pkgNamesQual0 byName0 src0 qual0
    res0 = ResImportMod { edge = import0.uid, from = src0.node, target = target0 }
    errAcc1 = foldl' (flip (:)) errAcc0 errs0
  in
  (res0 : resAcc0, errAcc1)

targetImportResolveMod
  :: Set.Set Text
  -> Map.Map ModName [CandResolveMod]
  -> CandResolveMod
  -> QualImportModName
  -> (TargetResolveMod, [ErrResolveMod])
targetImportResolveMod pkgNamesQual0 byName0 src0 qual0 =
  case qual0.pkgMay of
    Nothing -> targetUnqualResolveMod byName0 src0 qual0
    Just pkgName0 -> targetQualResolveMod pkgNamesQual0 byName0 src0 qual0 pkgName0

targetUnqualResolveMod
  :: Map.Map ModName [CandResolveMod]
  -> CandResolveMod
  -> QualImportModName
  -> (TargetResolveMod, [ErrResolveMod])
targetUnqualResolveMod byName0 src0 qual0 =
  let
    universe0 = Map.findWithDefault [] qual0.modName byName0
    samePkgVis0 = filter (samePkgVisibleResolveMod src0) universe0
    depVis0 = filter (depVisibleResolveMod src0) universe0
    regVis0 = filter (regVisibleResolveMod src0) universe0
    virVis0 = filter (virVisibleResolveMod src0) universe0

    samePkgHid0 = filter (samePkgHiddenResolveMod src0) universe0
    depHid0 = filter (depHiddenResolveMod src0) universe0
    regHid0 = filter (regHiddenResolveMod src0) universe0
    virHid0 = filter (virHiddenResolveMod src0) universe0
  in
  case firstNonEmptyResolveMod [samePkgVis0, depVis0, regVis0, virVis0] of
    Just bucket0 -> (targetBucketResolveMod bucket0, [])
    Nothing ->
      case firstNonEmptyResolveMod [samePkgHid0, depHid0, regHid0, virHid0] of
        Just bucket0 -> (HiddenTargetResolveMod (headNodeResolveMod bucket0), [HiddenImportErrResolveMod qual0.modName])
        Nothing -> (MissingTargetResolveMod, [])

targetQualResolveMod
  :: Set.Set Text
  -> Map.Map ModName [CandResolveMod]
  -> CandResolveMod
  -> QualImportModName
  -> Text
  -> (TargetResolveMod, [ErrResolveMod])
targetQualResolveMod pkgNamesQual0 byName0 src0 qual0 pkgName0 =
  let
    srcPkgName0 = src0.header.origin.pkg.name
  in
  if pkgName0 == srcPkgName0
    then
      (MissingTargetResolveMod, [InvalidImportPkgErrResolveMod qual0])
    else if not (Set.member pkgName0 pkgNamesQual0)
      then
        (MissingTargetResolveMod, [InvalidImportPkgErrResolveMod qual0])
      else
        let
          universe0 = filter (\cand0 -> cand0.header.origin.pkg.name == pkgName0) (Map.findWithDefault [] qual0.modName byName0)
          depVis0 = filter depVisibleQualResolveMod universe0
          regVis0 = filter regVisibleQualResolveMod universe0
          depHid0 = filter depHiddenQualResolveMod universe0
          regHid0 = filter regHiddenQualResolveMod universe0
        in
        case firstNonEmptyResolveMod [depVis0, regVis0] of
          Just bucket0 -> (targetBucketResolveMod bucket0, [])
          Nothing ->
            case firstNonEmptyResolveMod [depHid0, regHid0] of
              Just bucket0 -> (HiddenTargetResolveMod (headNodeResolveMod bucket0), [HiddenImportErrResolveMod qual0.modName])
              Nothing -> (MissingTargetResolveMod, [])

targetBucketResolveMod :: [CandResolveMod] -> TargetResolveMod
targetBucketResolveMod bucket0 =
  case bucket0 of
    [] -> MissingTargetResolveMod
    [cand0] -> TargetNodeResolveMod cand0.node
    _ -> AmbiguousTargetResolveMod (Vector.fromList (fmap (\cand0 -> cand0.node) bucket0))

headNodeResolveMod :: [CandResolveMod] -> NodeId
headNodeResolveMod bucket0 =
  case bucket0 of
    cand0 : _ -> cand0.node
    [] -> error "Fuddle.Compiler.ModGraph.Resolve.headNodeResolveMod: empty bucket"

firstNonEmptyResolveMod :: [[a]] -> Maybe [a]
firstNonEmptyResolveMod buckets0 =
  case List.dropWhile null buckets0 of
    bucket0 : _ -> Just bucket0
    [] -> Nothing

samePkgVisibleResolveMod :: CandResolveMod -> CandResolveMod -> Bool
samePkgVisibleResolveMod src0 cand0 =
  samePkgResolveMod src0 cand0 && visibleCandResolveMod cand0

samePkgHiddenResolveMod :: CandResolveMod -> CandResolveMod -> Bool
samePkgHiddenResolveMod src0 cand0 =
  samePkgResolveMod src0 cand0 && hiddenCandResolveMod cand0

depVisibleResolveMod :: CandResolveMod -> CandResolveMod -> Bool
depVisibleResolveMod src0 cand0 =
  not (samePkgResolveMod src0 cand0)
    && cand0.scope == DependencyScopeModGraph
    && cand0.header.origin.rootKind == PackageRootMod
    && visibleCandResolveMod cand0

depHiddenResolveMod :: CandResolveMod -> CandResolveMod -> Bool
depHiddenResolveMod src0 cand0 =
  not (samePkgResolveMod src0 cand0)
    && cand0.header.origin.rootKind == PackageRootMod
    && hiddenCandResolveMod cand0

regVisibleResolveMod :: CandResolveMod -> CandResolveMod -> Bool
regVisibleResolveMod src0 cand0 =
  not (samePkgResolveMod src0 cand0)
    && cand0.scope == DependencyScopeModGraph
    && cand0.header.origin.rootKind == RegistryRootMod
    && visibleCandResolveMod cand0

regHiddenResolveMod :: CandResolveMod -> CandResolveMod -> Bool
regHiddenResolveMod src0 cand0 =
  not (samePkgResolveMod src0 cand0)
    && cand0.header.origin.rootKind == RegistryRootMod
    && hiddenCandResolveMod cand0

virVisibleResolveMod :: CandResolveMod -> CandResolveMod -> Bool
virVisibleResolveMod src0 cand0 =
  not (samePkgResolveMod src0 cand0)
    && cand0.header.origin.rootKind == VirtualRootMod
    && visibleCandResolveMod cand0

virHiddenResolveMod :: CandResolveMod -> CandResolveMod -> Bool
virHiddenResolveMod src0 cand0 =
  not (samePkgResolveMod src0 cand0)
    && cand0.header.origin.rootKind == VirtualRootMod
    && hiddenCandResolveMod cand0

depVisibleQualResolveMod :: CandResolveMod -> Bool
depVisibleQualResolveMod cand0 =
  cand0.scope == DependencyScopeModGraph
    && cand0.header.origin.rootKind == PackageRootMod
    && visibleCandResolveMod cand0

depHiddenQualResolveMod :: CandResolveMod -> Bool
depHiddenQualResolveMod cand0 =
  cand0.header.origin.rootKind == PackageRootMod
    && hiddenCandResolveMod cand0

regVisibleQualResolveMod :: CandResolveMod -> Bool
regVisibleQualResolveMod cand0 =
  cand0.scope == DependencyScopeModGraph
    && cand0.header.origin.rootKind == RegistryRootMod
    && visibleCandResolveMod cand0

regHiddenQualResolveMod :: CandResolveMod -> Bool
regHiddenQualResolveMod cand0 =
  cand0.header.origin.rootKind == RegistryRootMod
    && hiddenCandResolveMod cand0

samePkgResolveMod :: CandResolveMod -> CandResolveMod -> Bool
samePkgResolveMod left0 right0 =
  left0.header.origin.pkg.uid == right0.header.origin.pkg.uid

visibleCandResolveMod :: CandResolveMod -> Bool
visibleCandResolveMod cand0 =
  cand0.scope /= HiddenScopeModGraph

hiddenCandResolveMod :: CandResolveMod -> Bool
hiddenCandResolveMod cand0 =
  cand0.scope == HiddenScopeModGraph