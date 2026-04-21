{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Sem.TypedIR
  ( ErrLowerTypedIR(..)
  , CtxLowerTypedIR(..)
  , ResLowerTypedIR(..)
  , lowerCheckedModTypedIR
  , validateTypedIRMod
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V

import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.ModGraph.Types (NodeId)
import Fuddle.Compiler.Sem.CoreTypes (CheckedModSem(..), ModeBack, StatusSem(..))

import Fuddle.Compiler.Back.Types (TypedIRMod)

import Fuddle.Compiler.Sem.TypedIR.Ann (ErrAnnTypedIR)
import Fuddle.Compiler.Sem.TypedIR.Decl (ErrDeclTypedIR)
import Fuddle.Compiler.Sem.TypedIR.Expr (ErrExprTypedIR)
import Fuddle.Compiler.Sem.TypedIR.Mod (ErrModTypedIR(..))
import Fuddle.Compiler.Sem.TypedIR.Pattern (ErrPatternTypedIR)
import Fuddle.Compiler.Sem.TypedIR.Ref (ErrRefTypedIR)
import Fuddle.Compiler.Sem.TypedIR.Scheme (ErrSchemeTypedIR)
import Fuddle.Compiler.Sem.TypedIR.Type (ErrTypeTypedIR)
import qualified Fuddle.Compiler.Sem.TypedIR.Validate as Validate

data CtxLowerTypedIR = CtxLowerTypedIR
  { modeBack :: !ModeBack
  , node :: !NodeId
  }

data ResLowerTypedIR = ResLowerTypedIR
  { typeIr :: !TypedIRMod
  }
  deriving stock (Eq, Show)

data ErrLowerTypedIR
  = RefErrLowerTypedIR !ErrRefTypedIR
  | TypeErrLowerTypedIR !ErrTypeTypedIR
  | SchemeErrLowerTypedIR !ErrSchemeTypedIR
  | PatternErrLowerTypedIR !ErrPatternTypedIR
  | ExprErrLowerTypedIR !ErrExprTypedIR
  | DeclErrLowerTypedIR !ErrDeclTypedIR
  | AnnErrLowerTypedIR !ErrAnnTypedIR
  | ModErrLowerTypedIR !ErrModTypedIR
  | ValidateErrLowerTypedIR !(Vector Diag)
  deriving stock (Eq, Show)

lowerCheckedModTypedIR :: ModeBack -> CheckedModSem -> Either ErrLowerTypedIR TypedIRMod
lowerCheckedModTypedIR _modeBack0 checked0 = do
  ensureReadyTypedIR checked0
  let typeIr0 = checked0.typeIr
  ensureValidTypedIR typeIr0
  pure typeIr0

validateTypedIRMod :: TypedIRMod -> Vector Diag
validateTypedIRMod = Validate.validateTypedIRMod

ensureReadyTypedIR :: CheckedModSem -> Either ErrLowerTypedIR ()
ensureReadyTypedIR checked0 =
  case checked0.status of
    ReadyStatusSem -> Right ()
    status0 ->
      Left
        (ModErrLowerTypedIR
          (UnsupportedModuleShapeErrModTypedIR
            ("semantic module is not ready for TypedIR handoff: " <> showTextTypedIR status0)
          )
        )

ensureValidTypedIR :: TypedIRMod -> Either ErrLowerTypedIR ()
ensureValidTypedIR typeIr0 =
  let diags0 = validateTypedIRMod typeIr0
  in
  if V.null diags0
    then Right ()
    else Left (ValidateErrLowerTypedIR diags0)

showTextTypedIR :: Show a => a -> Text
showTextTypedIR = Text.pack . show