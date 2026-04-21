module Fuddle.Compiler.Sem.Validate
  ( validateSem
  ) where

import Data.Vector (Vector)

import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.Sem.CoreTypes

validateSem :: TypedIRMod -> Vector Diag
validateSem mod = undefined