{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuddle.Compiler.Sem.CoreTypes
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
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Data.IntMap.Strict (IntMap)

import Fuddle.Compiler.Base.Core (Hash64(..))
import Fuddle.Compiler.Base.Range (Range(..))
import Fuddle.Compiler.Base.Diag (Diag(..))

newtype ModId = ModId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype DeclId = DeclId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype ExprId = ExprId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype BindId = BindId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype LocalId = LocalId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype AltId = AltId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype TyCanonId = TyCanonId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype SchemeId = SchemeId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype DataId = DataId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype CtorId = CtorId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype EffectId = EffectId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype OpId = OpId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype ForeignId = ForeignId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

newtype AliasId = AliasId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum)

data RefTypedIR
  = LocalRefTypedIR !LocalId
  | GlobalRefTypedIR !DeclId
  | CtorRefTypedIR !CtorId
  | EffectRefTypedIR !EffectId
  | OpRefTypedIR !OpId
  | ForeignRefTypedIR !ForeignId
  | RuntimeRefTypedIR !Text
  deriving stock (Eq, Ord, Show)

data VisTypedIR
  = PublicVisTypedIR
  | PrivateVisTypedIR
  deriving stock (Eq, Ord, Show)

data AssocTypedIR
  = LeftAssocTypedIR
  | RightAssocTypedIR
  | NonAssocTypedIR
  deriving stock (Eq, Ord, Show)

data RecTypedIR
  = NonRecTypedIR
  | RecTypedIR
  deriving stock (Eq, Ord, Show)

data PurityTypedIR
  = PurePurityTypedIR
  | EffectPurityTypedIR !(Vector EffectId)
  deriving stock (Eq, Show)

data MetaTypedIR = MetaTypedIR
  { name :: !Text
  , path :: !Text
  , pkg :: !Text
  , hashSource :: !Hash64
  , hashIface :: !Hash64
  }
  deriving stock (Eq, Show)

data DepTypedIR = DepTypedIR
  { name :: !Text
  , modId :: !ModId
  , hashIface :: !Hash64
  }
  deriving stock (Eq, Show)

data ExportTypedIR
  = ValueExportTypedIR !DeclId
  | DataExportTypedIR !DataId
  | CtorExportTypedIR !CtorId
  | AliasExportTypedIR !AliasId
  | EffectExportTypedIR !EffectId
  | ForeignExportTypedIR !ForeignId
  deriving stock (Eq, Show)

data EntryTypedIR
  = MainEntryTypedIR !DeclId
  | ViewEntryTypedIR !DeclId
  | NoneEntryTypedIR
  deriving stock (Eq, Show)

data TypedIRMod = TypedIRMod
  { uid :: !ModId
  , meta :: !MetaTypedIR
  , deps :: !(Vector DepTypedIR)
  , exports :: !(Vector ExportTypedIR)

  , aliases :: !(Vector AliasDeclTypedIR)
  , dataDecls :: !(Vector DataDeclTypedIR)
  , effectDecls :: !(Vector EffectDeclTypedIR)
  , foreignDecls :: !(Vector ForeignDeclTypedIR)
  , fixities :: !(Vector FixityDeclTypedIR)
  , valueGroups :: !(Vector GroupDeclTypedIR)

  , tys :: !(IntMap TyCanon)
  , schemes :: !(IntMap SchemeTypedIR)

  , entry :: !EntryTypedIR
  , anns :: !AnnTableTypedIR
  }
  deriving stock (Eq, Show)

data GroupDeclTypedIR = GroupDeclTypedIR
  { rec :: !RecTypedIR
  , decls :: !(Vector ValueDeclTypedIR)
  }
  deriving stock (Eq, Show)

data ValueDeclTypedIR = ValueDeclTypedIR
  { uid :: !DeclId
  , vis :: !VisTypedIR
  , name :: !Text
  , scheme :: !SchemeId
  , params :: !(Vector ParamTypedIR)
  , body :: !ExprTypedIR
  , purity :: !PurityTypedIR
  }
  deriving stock (Eq, Show)

data ParamTypedIR
  = VarParamTypedIR !LocalId !Text
  | PatParamTypedIR !PatTypedIR
  deriving stock (Eq, Show)

data DataDeclTypedIR = DataDeclTypedIR
  { uid :: !DeclId
  , vis :: !VisTypedIR
  , name :: !Text
  , ref :: !DataId
  , vars :: !(Vector QuantTypedIR)
  , ctors :: !(Vector CtorDeclTypedIR)
  }
  deriving stock (Eq, Show)

data CtorDeclTypedIR = CtorDeclTypedIR
  { uid :: !CtorId
  , name :: !Text
  , owner :: !DataId
  , fields :: !(Vector FieldDeclTypedIR)
  , resultTy :: !TyCanonId
  }
  deriving stock (Eq, Show)

data FieldDeclTypedIR = FieldDeclTypedIR
  { nameMay :: !(Maybe Text)
  , ty :: !TyCanonId
  , strict :: !Bool
  }
  deriving stock (Eq, Show)

data AliasDeclTypedIR = AliasDeclTypedIR
  { uid :: !DeclId
  , vis :: !VisTypedIR
  , name :: !Text
  , ref :: !AliasId
  , vars :: !(Vector QuantTypedIR)
  , rhs :: !TyCanonId
  }
  deriving stock (Eq, Show)

data EffectDeclTypedIR = EffectDeclTypedIR
  { uid :: !DeclId
  , vis :: !VisTypedIR
  , name :: !Text
  , ref :: !EffectId
  , vars :: !(Vector QuantTypedIR)
  , ops :: !(Vector OpDeclTypedIR)
  }
  deriving stock (Eq, Show)

data OpDeclTypedIR = OpDeclTypedIR
  { uid :: !OpId
  , name :: !Text
  , owner :: !EffectId
  , scheme :: !SchemeId
  , canFail :: !Bool
  }
  deriving stock (Eq, Show)

data ForeignDeclTypedIR = ForeignDeclTypedIR
  { uid :: !DeclId
  , vis :: !VisTypedIR
  , name :: !Text
  , ref :: !ForeignId
  , scheme :: !SchemeId
  , host :: !Text
  , sym :: !Text
  , pureMay :: !(Maybe Bool)
  }
  deriving stock (Eq, Show)

data FixityDeclTypedIR = FixityDeclTypedIR
  { uid :: !DeclId
  , name :: !Text
  , assoc :: !AssocTypedIR
  , prec :: !Int
  }
  deriving stock (Eq, Show)

data QuantTypedIR = QuantTypedIR
  { name :: !Text
  , idx :: !Int
  }
  deriving stock (Eq, Ord, Show)

data PredTypedIR
  = HasEffectPredTypedIR !EffectId
  | SatisfyPredTypedIR !Text !TyCanonId
  deriving stock (Eq, Show)

data SchemeTypedIR = SchemeTypedIR
  { uid :: !SchemeId
  , quants :: !(Vector QuantTypedIR)
  , preds :: !(Vector PredTypedIR)
  , body :: !TyCanonId
  }
  deriving stock (Eq, Show)

data TyFieldCanon = TyFieldCanon
  { name :: !Text
  , ty :: !TyCanonId
  }
  deriving stock (Eq, Show)

data TyRowCanon
  = EmptyRowTyCanon
  | ExtendRowTyCanon !(Vector TyFieldCanon) !(Maybe TyCanonId)
  deriving stock (Eq, Show)

data TyCanon
  = VarTyCanon !Int
  | NamedTyCanon !Text
  | AppTyCanon !TyCanonId !(Vector TyCanonId)
  | FunTyCanon !(Vector TyCanonId) !TyCanonId
  | TupleTyCanon !(Vector TyCanonId)
  | RecordTyCanon !TyRowCanon
  | ListTyCanon !TyCanonId
  | EffectTyCanon !(Vector EffectId) !TyCanonId
  | ForallTyCanon !(Vector QuantTypedIR) !(Vector PredTypedIR) !TyCanonId
  deriving stock (Eq, Show)

data LitTypedIR
  = IntLitTypedIR !Integer
  | FloatLitTypedIR !Double
  | CharLitTypedIR !Char
  | StringLitTypedIR !Text
  | BoolLitTypedIR !Bool
  | UnitLitTypedIR
  deriving stock (Eq, Show)

data PatTypedIR
  = WildPatTypedIR
  | VarPatTypedIR !LocalId !Text
  | AsPatTypedIR !LocalId !Text !PatTypedIR
  | LitPatTypedIR !LitTypedIR
  | CtorPatTypedIR !CtorId !(Vector PatTypedIR)
  | RecordPatTypedIR !(Vector (Text, PatTypedIR))
  | TuplePatTypedIR !(Vector PatTypedIR)
  | ListPatTypedIR !(Vector PatTypedIR)
  deriving stock (Eq, Show)

data BindTypedIR
  = VarBindTypedIR !BindId !LocalId !ExprTypedIR
  | PatBindTypedIR !BindId !PatTypedIR !ExprTypedIR
  deriving stock (Eq, Show)

data AltCaseTypedIR = AltCaseTypedIR
  { uid :: !AltId
  , pat :: !PatTypedIR
  , guards :: !(Vector GuardTypedIR)
  , body :: !ExprTypedIR
  }
  deriving stock (Eq, Show)

data GuardTypedIR = GuardTypedIR
  { uid :: !AltId
  , test :: !ExprTypedIR
  , body :: !ExprTypedIR
  }
  deriving stock (Eq, Show)

data HandlerCatchTypedIR = HandlerCatchTypedIR
  { uid :: !AltId
  , pat :: !PatTypedIR
  , body :: !ExprTypedIR
  }
  deriving stock (Eq, Show)

data ExprTypedIR
  = RefExprTypedIR !ExprId !RefTypedIR
  | LitExprTypedIR !ExprId !LitTypedIR

  | LambdaExprTypedIR !ExprId !(Vector ParamTypedIR) !ExprTypedIR
  | ApplyExprTypedIR !ExprId !ExprTypedIR !(Vector ExprTypedIR)

  | LetExprTypedIR !ExprId !(Vector BindTypedIR) !ExprTypedIR
  | IfExprTypedIR !ExprId !ExprTypedIR !ExprTypedIR !ExprTypedIR
  | CaseExprTypedIR !ExprId !ExprTypedIR !(Vector AltCaseTypedIR)

  | DoExprTypedIR !ExprId !(Vector BindTypedIR) !ExprTypedIR
  | CatchExprTypedIR !ExprId !ExprTypedIR !(Vector HandlerCatchTypedIR)

  | CtorExprTypedIR !ExprId !CtorId !(Vector ExprTypedIR)
  | RecordExprTypedIR !ExprId !(Vector FieldSetTypedIR)
  | UpdateExprTypedIR !ExprId !ExprTypedIR !(Vector FieldSetTypedIR)
  | AccessExprTypedIR !ExprId !ExprTypedIR !Text

  | ListExprTypedIR !ExprId !(Vector ExprTypedIR)
  | TupleExprTypedIR !ExprId !(Vector ExprTypedIR)

  | ForeignExprTypedIR !ExprId !ForeignId !(Vector ExprTypedIR)
  | PrimExprTypedIR !ExprId !Text !(Vector ExprTypedIR)

  | MarkupExprTypedIR !ExprId !MarkupTypedIR

  | HoleExprTypedIR !ExprId !Text
  | NoteExprTypedIR !ExprId !OriginTypedIR !ExprTypedIR
  deriving stock (Eq, Show)

data FieldSetTypedIR = FieldSetTypedIR
  { name :: !Text
  , value :: !ExprTypedIR
  }
  deriving stock (Eq, Show)

data MarkupTypedIR =
    FragmentMarkupTypedIR !(Vector ChildMarkupTypedIR)
  | ElemMarkupTypedIR !ElemMarkupCT
  deriving stock (Eq, Show)

data ElemMarkupCT = ElemMarkupCT
  { tag :: !Text
  , attrs :: !(Vector AttrMarkupTypedIR)
  , kids :: !(Vector ChildMarkupTypedIR)
  , keyed :: !Bool
  }
  deriving stock (Eq, Show)

data AttrMarkupTypedIR
  = StaticAttrMarkupTypedIR !Text !Text
  | DynamicAttrMarkupTypedIR !Text !ExprTypedIR
  | EventAttrMarkupTypedIR !Text !ExprTypedIR
  deriving stock (Eq, Show)

data ChildMarkupTypedIR
  = TextChildMarkupTypedIR !Text
  | ExprChildMarkupTypedIR !ExprTypedIR
  | NodeChildMarkupTypedIR !MarkupTypedIR
  deriving stock (Eq, Show)

data OriginTypedIR = OriginTypedIR
  { rangeMay :: !(Maybe Range)
  , noteMay :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

data AnnExprTypedIR = AnnExprTypedIR
  { ty :: !TyCanonId
  , schemeMay :: !(Maybe SchemeId)
  , purity :: !PurityTypedIR
  , origin :: !OriginTypedIR
  }
  deriving stock (Eq, Show)

data AnnBindTypedIR = AnnBindTypedIR
  { ty :: !TyCanonId
  , origin :: !OriginTypedIR
  }
  deriving stock (Eq, Show)

data AnnDeclTypedIR = AnnDeclTypedIR
  { scheme :: !SchemeId
  , purity :: !PurityTypedIR
  , origin :: !OriginTypedIR
  }
  deriving stock (Eq, Show)

data AnnTableTypedIR = AnnTableTypedIR
  { exprs :: !(IntMap AnnExprTypedIR)
  , binds :: !(IntMap AnnBindTypedIR)
  , decls :: !(IntMap AnnDeclTypedIR)
  }
  deriving stock (Eq, Show)

data ErrLowerTypedIR
  = UnresolvedRefErrTypedIR !Text
  | MissingTypeErrTypedIR !ExprId
  | MissingSchemeErrTypedIR !DeclId
  | InvalidRecGroupErrTypedIR !Text
  | InvalidEffectInfoErrTypedIR !ExprId
  | InvalidMarkupErrTypedIR !Text
  | UnsupportedCheckedShapeErrTypedIR !Text
  deriving stock (Eq, Show)
