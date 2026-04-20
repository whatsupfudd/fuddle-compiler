{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.ModGraph.Invalidate
  ( FrontierModGraph(..)
  , PlanInvalidateMod(..)
  , mkPlanInvalidateMod
  ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Fuddle.Compiler.ModGraph.Types (NodeId, RootId)

data FrontierModGraph = FrontierModGraph
  { headers :: !(Vector NodeId)
  , ifaces :: !(Vector NodeId)
  , bodies :: !(Vector NodeId)
  , emits :: !(Vector NodeId)
  }
  deriving stock (Eq, Show)

data PlanInvalidateMod = PlanInvalidateMod
  { dirtyRoots :: !(Vector RootId)
  , dirtyNodes :: !(Vector NodeId)
  , frontier :: !FrontierModGraph
  }
  deriving stock (Eq, Show)

-- Conservative stage closure:
-- headers ⊆ bodies ⊆ ifaces ⊆ emits
--
-- This keeps the plan scheduling-safe:
-- * a header refresh implies a body refresh
-- * a body refresh implies interface refresh
-- * an interface refresh implies emit refresh
--
-- Callers can still seed only later stages when they know earlier work has
-- already happened in the current cycle.
mkPlanInvalidateMod :: Vector RootId -> Vector NodeId -> FrontierModGraph -> PlanInvalidateMod
mkPlanInvalidateMod dirtyRoots0 dirtyNodes0 frontier0 =
  let
    headers1 = normVec frontier0.headers
    bodies1 = unionVecs [frontier0.bodies, headers1]
    ifaces1 = unionVecs [frontier0.ifaces, bodies1]
    emits1 = unionVecs [frontier0.emits, ifaces1]

    frontier1 = FrontierModGraph
      { headers = headers1
      , ifaces = ifaces1
      , bodies = bodies1
      , emits = emits1
      }

    dirtyNodes1 = unionVecs [dirtyNodes0, headers1, bodies1, ifaces1, emits1]
  in
  PlanInvalidateMod
    { dirtyRoots = normVec dirtyRoots0
    , dirtyNodes = dirtyNodes1
    , frontier = frontier1
    }

normVec :: Ord a => Vector a -> Vector a
normVec xs = Vector.fromList (Set.toAscList (setVec xs))

unionVecs :: Ord a => [Vector a] -> Vector a
unionVecs xss = Vector.fromList (Set.toAscList (foldl' step Set.empty xss))
  where
    step :: Ord a => Set a -> Vector a -> Set a
    step acc xs = Set.union acc (setVec xs)

setVec :: Ord a => Vector a -> Set a
setVec xs = Vector.foldl' (flip Set.insert) Set.empty xs