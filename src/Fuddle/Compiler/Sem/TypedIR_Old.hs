{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Fuddle.Compiler.Sem.TypedIR_Old
  ( ModId(..)
  , DeclId(..)
  , ExprId(..)
  , BindId(..)
  , LocalId(..)
  , AltId(..)
  , TyCanonId(..)
  , SchemeId(..)
  , DataId(..)
  , CtorId(..)
  , EffectId(..)
  , OpId(..)
  , ForeignId(..)
  , AliasId(..)

  , RefTypedIR(..)
  , VisTypedIR(..)
  , AssocTypedIR(..)
  , RecTypedIR(..)
  , PurityTypedIR(..)

  , MetaTypedIR(..)
  , DepTypedIR(..)
  , ExportTypedIR(..)
  , EntryTypedIR(..)

  , TypedIRMod(..)
  , GroupDeclTypedIR(..)

  , ValueDeclTypedIR(..)
  , DataDeclTypedIR(..)
  , CtorDeclTypedIR(..)
  , FieldDeclTypedIR(..)
  , AliasDeclTypedIR(..)
  , EffectDeclTypedIR(..)
  , OpDeclTypedIR(..)
  , ForeignDeclTypedIR(..)
  , FixityDeclTypedIR(..)

  , ParamTypedIR(..)
  , BindTypedIR(..)
  , AltCaseTypedIR(..)
  , GuardTypedIR(..)
  , HandlerCatchTypedIR(..)

  , ExprTypedIR(..)
  , PatTypedIR(..)
  , LitTypedIR(..)
  , FieldSetTypedIR(..)
  , MarkupTypedIR(..)
  , ElemMarkupCT(..)
  , AttrMarkupTypedIR(..)
  , ChildMarkupTypedIR(..)

  , TyCanon(..)
  , TyRowCanon(..)
  , TyFieldCanon(..)
  , SchemeTypedIR(..)
  , QuantTypedIR(..)
  , PredTypedIR(..)

  , AnnExprTypedIR(..)
  , AnnBindTypedIR(..)
  , AnnDeclTypedIR(..)
  , AnnTableTypedIR(..)
  , OriginTypedIR(..)

  , ErrLowerTypedIR(..)
  , lowerCheckedModTypedIR
  , validateTypedIRMod
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Data.IntMap.Strict (IntMap)

import Fuddle.Compiler.Base.Core (Hash64(..))
import Fuddle.Compiler.Base.Range (Range(..))
import Fuddle.Compiler.Base.Diag (Diag(..))

import Fuddle.Compiler.ModGraph.Types (NodeId)

import Fuddle.Compiler.Sem.CoreTypes
-- import Fuddle.Compiler.Sem.Effect (EffSem)
-- import Fuddle.Compiler.Sem.Id (ModSemId)
-- import Fuddle.Compiler.Sem.Interface (UnitIfaceSem)
-- import Fuddle.Compiler.Sem.Use (UseSummarySem)


data ModeBack

data StageSem =
    ResolveStageSem
  | InferStageSem
  | SolveStageSem
  | GeneralizeStageSem
  | InterfaceStageSem
  | TypedIRStageSem
  deriving stock (Eq, Ord, Show)

data StatusSem =
    ReadyStatusSem
  | FailedStatusSem !StageSem
  deriving stock (Eq, Show)

data PuritySem =
    PurePuritySem
  | EffectPuritySem !EffSem
  deriving stock (Eq, Show)


data CheckedModSem = CheckedModSem {
    uid :: !ModSemId
  , node :: !NodeId
  , status :: !StatusSem

  , unitIface :: !UnitIfaceSem
  , useSummary :: !UseSummarySem
  , typeIr :: !TypedIRMod
  }
  deriving stock (Eq, Show)

-- TODO: implement these functions:
lowerCheckedModTypedIR :: ModeBack -> CheckedModSem -> Either ErrLowerTypedIR TypedIRMod
lowerCheckedModTypedIR mode back = undefined

validateTypedIRMod :: TypedIRMod -> Vector Diag
validateTypedIRMod mod = undefined
