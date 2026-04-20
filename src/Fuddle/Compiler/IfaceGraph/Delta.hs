{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Fuddle.Compiler.IfaceGraph.Delta
  ( ChangeExportIfaceGraph(..)
  , DeltaIfaceGraph(..)
  , diffIfaceGraph
  , affectsUseIfaceGraph
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Fuddle.Compiler.Base.Core (Hash64)
import Fuddle.Compiler.IfaceGraph.Export (ExportIfaceGraph(..), ExportKeyIfaceGraph(..))
import Fuddle.Compiler.IfaceGraph.Use (UseIfaceGraph(..))
import Fuddle.Compiler.ModGraph (NodeId)
import GHC.Records (HasField)

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

diffIfaceGraph
  :: ( HasField "node" unit NodeId
     , HasField "hashApi" unit Hash64
     , HasField "hashNamespace" unit Hash64
     , HasField "hashInline" unit Hash64
     , HasField "exports" unit (Vector ExportIfaceGraph)
     )
  => unit
  -> unit
  -> DeltaIfaceGraph
diffIfaceGraph unitPrev unitNext =
  let
    nodePrev = unitPrev.node
    nodeNext = unitNext.node
  in
  if nodePrev /= nodeNext
    then error (errDelta "diffIfaceGraph" ("node mismatch: " <> show nodePrev <> " vs " <> show nodeNext))
    else DeltaIfaceGraph
      { node = nodeNext
      , apiChanged = unitPrev.hashApi /= unitNext.hashApi
      , namespaceChanged = unitPrev.hashNamespace /= unitNext.hashNamespace
      , inlineChanged = unitPrev.hashInline /= unitNext.hashInline
      , exportsChanged = diffExportsDeltaIfaceGraph unitPrev.exports unitNext.exports
      }

affectsUseIfaceGraph :: DeltaIfaceGraph -> Vector UseIfaceGraph -> Bool
affectsUseIfaceGraph delta0 uses0 =
  let
    keysChanged0 = keysChangedDeltaIfaceGraph delta0.exportsChanged
  in
  Vector.any (affectsUse1IfaceGraph delta0 keysChanged0) uses0

affectsUse1IfaceGraph :: DeltaIfaceGraph -> Set ExportKeyIfaceGraph -> UseIfaceGraph -> Bool
affectsUse1IfaceGraph delta0 keysChanged0 use0 =
  case use0 of
    NamespaceUseIfaceGraph -> delta0.namespaceChanged
    AllUseIfaceGraph -> delta0.apiChanged
    ValueUseIfaceGraph name0 -> Set.member (ValueKeyIfaceGraph name0) keysChanged0
    TypeUseIfaceGraph name0 -> Set.member (TypeKeyIfaceGraph name0) keysChanged0
    CtorUseIfaceGraph name0 -> Set.member (CtorKeyIfaceGraph name0) keysChanged0
    AliasUseIfaceGraph name0 -> Set.member (AliasKeyIfaceGraph name0) keysChanged0
    EffectUseIfaceGraph name0 -> Set.member (EffectKeyIfaceGraph name0) keysChanged0
    ForeignUseIfaceGraph name0 -> Set.member (ForeignKeyIfaceGraph name0) keysChanged0

diffExportsDeltaIfaceGraph
  :: Vector ExportIfaceGraph
  -> Vector ExportIfaceGraph
  -> Vector ChangeExportIfaceGraph
diffExportsDeltaIfaceGraph exportsPrev exportsNext =
  let
    mapPrev = mapExportsDeltaIfaceGraph exportsPrev
    mapNext = mapExportsDeltaIfaceGraph exportsNext
    keysAll = Set.toAscList (Map.keysSet mapPrev `Set.union` Map.keysSet mapNext)
  in
  Vector.fromList (mapMaybe (diffExportKeyDeltaIfaceGraph mapPrev mapNext) keysAll)

diffExportKeyDeltaIfaceGraph
  :: Map ExportKeyIfaceGraph Hash64
  -> Map ExportKeyIfaceGraph Hash64
  -> ExportKeyIfaceGraph
  -> Maybe ChangeExportIfaceGraph
diffExportKeyDeltaIfaceGraph mapPrev mapNext key0 =
  case (Map.lookup key0 mapPrev, Map.lookup key0 mapNext) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just hashNext) -> Just (AddExportIfaceGraph key0 hashNext)
    (Just hashPrev, Nothing) -> Just (RemoveExportIfaceGraph key0 hashPrev)
    (Just hashPrev, Just hashNext)
      | hashPrev == hashNext -> Nothing
      | otherwise -> Just (UpdateExportIfaceGraph key0 hashPrev hashNext)

mapExportsDeltaIfaceGraph :: Vector ExportIfaceGraph -> Map ExportKeyIfaceGraph Hash64
mapExportsDeltaIfaceGraph exports0 =
  Vector.foldl' step Map.empty exports0
  where
    step acc export0 =
      let
        key0 = keyExportIfaceGraph export0
        hash0 = hashExportIfaceGraph export0
      in
      case Map.lookup key0 acc of
        Just _ -> error (errDelta "mapExportsDeltaIfaceGraph" ("duplicate export key: " <> show key0))
        Nothing -> Map.insert key0 hash0 acc

keysChangedDeltaIfaceGraph :: Vector ChangeExportIfaceGraph -> Set ExportKeyIfaceGraph
keysChangedDeltaIfaceGraph changes0 =
  Vector.foldl' step Set.empty changes0
  where
    step acc change0 = Set.insert (keyChangeExportIfaceGraph change0) acc

keyChangeExportIfaceGraph :: ChangeExportIfaceGraph -> ExportKeyIfaceGraph
keyChangeExportIfaceGraph change0 =
  case change0 of
    AddExportIfaceGraph key0 _ -> key0
    RemoveExportIfaceGraph key0 _ -> key0
    UpdateExportIfaceGraph key0 _ _ -> key0

keyExportIfaceGraph :: ExportIfaceGraph -> ExportKeyIfaceGraph
keyExportIfaceGraph export0 =
  case export0 of
    ValueExportIfaceGraph name0 _ -> ValueKeyIfaceGraph name0
    TypeExportIfaceGraph name0 _ -> TypeKeyIfaceGraph name0
    CtorExportIfaceGraph name0 _ -> CtorKeyIfaceGraph name0
    AliasExportIfaceGraph name0 _ -> AliasKeyIfaceGraph name0
    EffectExportIfaceGraph name0 _ -> EffectKeyIfaceGraph name0
    ForeignExportIfaceGraph name0 _ -> ForeignKeyIfaceGraph name0

hashExportIfaceGraph :: ExportIfaceGraph -> Hash64
hashExportIfaceGraph export0 =
  case export0 of
    ValueExportIfaceGraph _ hash0 -> hash0
    TypeExportIfaceGraph _ hash0 -> hash0
    CtorExportIfaceGraph _ hash0 -> hash0
    AliasExportIfaceGraph _ hash0 -> hash0
    EffectExportIfaceGraph _ hash0 -> hash0
    ForeignExportIfaceGraph _ hash0 -> hash0

errDelta :: String -> String -> String
errDelta fun msg = "Fuddle.Compiler.IfaceGraph.Delta." <> fun <> ": " <> msg