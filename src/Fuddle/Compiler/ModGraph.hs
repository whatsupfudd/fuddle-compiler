{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.ModGraph
  ( GraphId(..)
  , NodeId(..)
  , EdgeId(..)
  , SccId(..)
  , PkgId(..)
  , RootId(..)
  , ScopeModGraph(..)
  , StatusNodeModGraph(..)
  , StatusEdgeModGraph(..)
  , ModGraph(..)
  , BuildReqModGraph(..)
  , BuildResModGraph(..)
  , UpdateReqModGraph(..)
  , UpdateResModGraph(..)
  , ErrBuildModGraph(..)
  , buildModGraph
  , updateModGraph
  , validateModGraph
  ) where

import Data.Vector (Vector)
import Data.Text (Text)

import Fuddle.Compiler.Base.Diag (Diag)
import qualified Fuddle.Compiler.ModGraph.Build as Bm
import Fuddle.Compiler.ModGraph.Types
  ( GraphId(..)
  , NodeId(..)
  , EdgeId(..)
  , SccId(..)
  , PkgId(..)
  , RootId(..)
  , ScopeModGraph(..)
  , StatusNodeModGraph(..)
  , StatusEdgeModGraph(..)
  )
import qualified Fuddle.Compiler.ModGraph.Validate as Vm
import Fuddle.Compiler.ModGraph.Build (BuildReqModGraph, BuildResModGraph, UpdateReqModGraph, UpdateResModGraph)
import Fuddle.Compiler.ModGraph.GraphTypes (ModGraph)
import Fuddle.Compiler.ModGraph.Discover (ErrDiscoverMod)
import Fuddle.Compiler.ModGraph.Resolve (ErrResolveMod)
import Fuddle.Compiler.ModGraph.Header (ErrHeaderMod)

data ErrBuildModGraph
  = DiscoverErrBuildModGraph !ErrDiscoverMod
  | ResolveErrBuildModGraph !ErrResolveMod
  | HeaderErrBuildModGraph !ErrHeaderMod
  | InvariantErrBuildModGraph !Text
  deriving stock (Eq, Show)


buildModGraph :: BuildReqModGraph -> Either ErrBuildModGraph BuildResModGraph
buildModGraph = undefined

updateModGraph :: UpdateReqModGraph -> Either ErrBuildModGraph UpdateResModGraph
updateModGraph = undefined

validateModGraph :: ModGraph -> Vector Diag
validateModGraph = Vm.validateModGraph