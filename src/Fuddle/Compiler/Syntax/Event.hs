{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Syntax.Event
  ( ParseEvent(..)
  , EventErr(..)
  , validateEvents
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Fuddle.Compiler.Base.Diag (Diag)
import Fuddle.Compiler.Syntax.Kind (SyntaxKind, isNodeKd, isTokenKd)
import Fuddle.Compiler.Syntax.Token (SyntheticReason, TokIx(..))

data ParseEvent
  = StartPE !SyntaxKind
  | TokPE !TokIx
  | TokSyntheticPE !SyntaxKind !SyntheticReason
  | FinishPE
  | ErrorPE !Diag
  | TombPE
  deriving stock (Eq, Show)

data EventErr
  = RootMissingEE
  | RootExtraEE !Int !SyntaxKind
  | StartNodeExpectedEE !Int !SyntaxKind
  | SyntheticTokenExpectedEE !Int !SyntaxKind
  | TokOutsideNodeEE !Int !TokIx
  | SyntheticOutsideNodeEE !Int !SyntaxKind
  | TokNegativeEE !Int !TokIx
  | TokOrderEE !Int !TokIx !TokIx
  | FinishUnderflowEE !Int
  | NodeUnclosedEE !SyntaxKind
  deriving stock (Eq, Show)

validateEvents :: Vector ParseEvent -> Either (Vector EventErr) ()
validateEvents pes =
  let
    st = V.ifoldl' stepEvent initVE pes
    errs = finalErrs st
  in
  if V.null errs then Right () else Left errs

data StateVE = StateVE
  { stackVE :: ![SyntaxKind]
  , prevTokVE :: !(Maybe TokIx)
  , rootSeenVE :: !Bool
  , rootClosedVE :: !Bool
  , errsVE :: ![EventErr]
  }

initVE :: StateVE
initVE = StateVE { 
    stackVE = []
  , prevTokVE = Nothing
  , rootSeenVE = False
  , rootClosedVE = False
  , errsVE = []
  }

stepEvent :: StateVE -> Int -> ParseEvent -> StateVE
stepEvent st ix ev =
  case ev of
    StartPE kd -> stepStart ix kd st
    TokPE tok -> stepTok ix tok st
    TokSyntheticPE kd _reason -> stepSynthetic ix kd st
    FinishPE -> stepFinish ix st
    ErrorPE _diag -> st
    TombPE -> st

stepStart :: Int -> SyntaxKind -> StateVE -> StateVE
stepStart ix kd st =
  let
    st1 =
      if isNodeKd kd
        then st
        else addErr (StartNodeExpectedEE ix kd) st

    st2 =
      if hasOpenNode st1
        then st1
        else
          if st1.rootClosedVE
            then addErr (RootExtraEE ix kd) st1
            else st1 { rootSeenVE = True }
  in
  st2 { stackVE = kd : st2.stackVE, rootClosedVE = False }

stepTok :: Int -> TokIx -> StateVE -> StateVE
stepTok ix tok st =
  let
    st1 =
      if hasOpenNode st
        then st
        else addErr (TokOutsideNodeEE ix tok) st

    st2 =
      if isNegativeTok tok
        then addErr (TokNegativeEE ix tok) st1
        else st1

    st3 =
      case st2.prevTokVE of
        Just prev | not (isNegativeTok tok) && tok <= prev -> addErr (TokOrderEE ix prev tok) st2
        _ -> st2

    prevTok1 =
      if isNegativeTok tok
        then st3.prevTokVE
        else Just tok
  in
  st3 { prevTokVE = prevTok1 }

stepSynthetic :: Int -> SyntaxKind -> StateVE -> StateVE
stepSynthetic ix kd st =
  let
    st1 =
      if hasOpenNode st
        then st
        else addErr (SyntheticOutsideNodeEE ix kd) st

    st2 =
      if isTokenKd kd
        then st1
        else addErr (SyntheticTokenExpectedEE ix kd) st1
  in
  st2

stepFinish :: Int -> StateVE -> StateVE
stepFinish ix st =
  case st.stackVE of
    [] -> addErr (FinishUnderflowEE ix) st
    _kd : rest ->
      st
        { stackVE = rest
        , rootClosedVE = null rest
        }

hasOpenNode :: StateVE -> Bool
hasOpenNode st = not (null st.stackVE)

isNegativeTok :: TokIx -> Bool
isNegativeTok (TokIx raw) = raw < 0

addErr :: EventErr -> StateVE -> StateVE
addErr err st = st { errsVE = err : st.errsVE }

finalErrs :: StateVE -> Vector EventErr
finalErrs st =
  let
    errs0 = reverse st.errsVE
    errs1 =
      if st.rootSeenVE
        then errs0
        else errs0 <> [RootMissingEE]
    errs2 = errs1 <> fmap NodeUnclosedEE (reverse st.stackVE)
  in
  V.fromList errs2