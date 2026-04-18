{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Parse.Front
  ( ModeParse(..)
  , ParseRes(..)
  , parseTokens
  ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Fuddle.Compiler.Base.Diag
  ( CodeDiag(..)
  , Diag
  , SeverityDiag(..)
  , StageDiag(..)
  , mkDiag
  )
import Fuddle.Compiler.Base.Range
  ( Range
  , emptyRange
  , mergeRange
  )
import Fuddle.Compiler.Syntax.Event
  ( EventErr(..)
  , ParseEvent(..)
  , validateEvents
  )
import Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  , isTriviaKd
  )
import Fuddle.Compiler.Syntax.Token
  ( TokIx(..)
  , TokenLex(..)
  , TokenStream
  , toVectorToks
  )

data ModeParse
  = StrictPM
  | TolerantPM
  deriving stock (Eq, Ord, Show)

data ParseRes = ParseRes
  { eventsPR :: !(Vector ParseEvent)
  , diagsPR :: !(Vector Diag)
  , recoveredPR :: !Bool
  }
  deriving stock (Eq, Show)

data PhaseRoot
  = HeadPH
  | ImportPH
  | DeclPH
  deriving stock (Eq, Ord, Show)

data AccParse = AccParse
  { eventsRevPA :: ![ParseEvent]
  , diagsRevPA :: ![Diag]
  , recoveredPA :: !Bool
  , phasePA :: !PhaseRoot
  , moduleSeenPA :: !Bool
  }

data ScanParse = ScanParse
  { eventsSP :: !(Vector ParseEvent)
  , diagsSP :: !(Vector Diag)
  , recoveredSP :: !Bool
  }

data PlanRoot
  = EndPR
  | NeutralPR
  | NodePR !SyntaxKind !Int ![Diag] !Bool
  | SkipPR !Int ![Diag] !Bool

data SigDef
  = SigSD
  | DefSD
  | UnknownSD
  deriving stock (Eq, Ord, Show)

data DepthScan = DepthScan
  { parenDP :: !Int
  , bracketDP :: !Int
  , braceDP :: !Int
  , layoutDP :: !Int
  }
  deriving stock (Eq, Show)

parseTokens :: ModeParse -> TokenStream -> ParseRes
parseTokens mode0 stream0 =
  let toks0 = toVectorToks stream0
      scan0 = scanRoot mode0 toks0
  in
  case validateEvents scan0.eventsSP of
    Right () ->
      ParseRes
        { eventsPR = scan0.eventsSP
        , diagsPR = scan0.diagsSP
        , recoveredPR = scan0.recoveredSP
        }

    Left errs0 ->
      let diagsVal0 = V.map diagEventErr errs0
          diagsAll0 = scan0.diagsSP <> diagsVal0
      in
      ParseRes
        { eventsPR = fallbackEvents toks0 diagsAll0
        , diagsPR = diagsAll0
        , recoveredPR = True
        }

scanRoot :: ModeParse -> Vector TokenLex -> ScanParse
scanRoot mode0 toks0 =
  let acc0 =
        AccParse
          { eventsRevPA = [StartPE SourceFileNd]
          , diagsRevPA = []
          , recoveredPA = False
          , phasePA = HeadPH
          , moduleSeenPA = False
          }

      acc1 = goRoot 0 acc0
      acc2 = pushEvent FinishPE acc1
  in
  ScanParse
    { eventsSP = V.fromList (reverse (eventsRevPA acc2))
    , diagsSP = V.fromList (reverse (diagsRevPA acc2))
    , recoveredSP = recoveredPA acc2
    }
  where
    goRoot :: Int -> AccParse -> AccParse
    goRoot ix0 acc0 =
      case planRootAt mode0 toks0 ix0 (phasePA acc0) (moduleSeenPA acc0) of
        EndPR -> acc0

        NeutralPR ->
          goRoot (ix0 + 1) (pushEvent (TokPE (tokIxAt ix0)) acc0)

        NodePR kind0 endIx0 diags0 recovered0 ->
          let endIx1 = max (ix0 + 1) endIx0
              acc1 = pushDiags diags0 acc0
              acc2 = emitNode kind0 ix0 endIx1 acc1
              acc3 = acc2 { recoveredPA = recoveredPA acc2 || recovered0 }
              acc4 = advancePhase kind0 acc3
          in
          goRoot endIx1 acc4

        SkipPR endIx0 diags0 _ ->
          let endIx1 = max (ix0 + 1) endIx0
              acc1 = pushDiags diags0 acc0
              acc2 = emitNode SkippedNd ix0 endIx1 acc1
              acc3 = acc2 { recoveredPA = True }
          in
          goRoot endIx1 acc3

planRootAt :: ModeParse -> Vector TokenLex -> Int -> PhaseRoot -> Bool -> PlanRoot
planRootAt mode0 toks0 ix0 phase0 moduleSeen0
  | ix0 >= V.length toks0 = EndPR
  | kindTokAt toks0 ix0 == EndTk = EndPR
  | isNeutralRootKd (kindTokAt toks0 ix0) = NeutralPR
  | otherwise =
      case kindTokAt toks0 ix0 of
        ModuleKwTk ->
          let endIx0 = findNodeEnd toks0 ix0
              range0 = rangeSlice toks0 ix0 endIx0
          in
          NodePR ModuleDeclNd endIx0 (orderDiags ModuleDeclNd phase0 moduleSeen0 range0) False

        ImportKwTk ->
          let endIx0 = findNodeEnd toks0 ix0
              range0 = rangeSlice toks0 ix0 endIx0
          in
          NodePR ImportDeclNd endIx0 (orderDiags ImportDeclNd phase0 moduleSeen0 range0) False

        TypeKwTk ->
          let endIx0 = findNodeEnd toks0 ix0
              kind0 =
                case nextSignificantKd toks0 (ix0 + 1) of
                  Just AliasKwTk -> AliasDeclNd
                  _ -> TypeDeclNd
          in
          NodePR kind0 endIx0 [] False

        RegionKwTk ->
          NodePR RegionDeclNd (findNodeEnd toks0 ix0) [] False

        AnchorKwTk ->
          NodePR AnchorDeclNd (findNodeEnd toks0 ix0) [] False

        CellKwTk ->
          NodePR CellDeclNd (findNodeEnd toks0 ix0) [] False

        NativeKwTk ->
          NodePR NativeDeclNd (findNodeEnd toks0 ix0) [] False

        ForeignKwTk ->
          NodePR ForeignDeclNd (findNodeEnd toks0 ix0) [] False

        InfixKwTk ->
          NodePR FixityDeclNd (findNodeEnd toks0 ix0) [] False

        ThreadKwTk ->
          planThread mode0 toks0 ix0

        LowerNameTk ->
          planValue mode0 toks0 ix0

        _ ->
          let endIx0 = findSkipEnd mode0 toks0 ix0
              range0 = rangeSlice toks0 ix0 endIx0
          in
          SkipPR endIx0 [diagParse "PFR001" range0 "unexpected top-level syntax"] True

planThread :: ModeParse -> Vector TokenLex -> Int -> PlanRoot
planThread mode0 toks0 ix0 =
  let endIx0 = findNodeEnd toks0 ix0
      range0 = rangeSlice toks0 ix0 endIx0
  in
  case scanSigDef toks0 ix0 endIx0 of
    SigSD ->
      NodePR ThreadSigNd endIx0 [] False

    DefSD ->
      NodePR ThreadDeclNd endIx0 [] False

    UnknownSD ->
      case mode0 of
        StrictPM ->
          SkipPR endIx0 [diagParse "PFR005" range0 "cannot distinguish between a thread signature and a thread definition"] True

        TolerantPM ->
          NodePR ThreadDeclNd endIx0 [diagParse "PFR006" range0 "cannot distinguish between a thread signature and a thread definition; treating it as a thread definition"] True

planValue :: ModeParse -> Vector TokenLex -> Int -> PlanRoot
planValue mode0 toks0 ix0 =
  let endIx0 = findNodeEnd toks0 ix0
      range0 = rangeSlice toks0 ix0 endIx0
  in
  case scanSigDef toks0 ix0 endIx0 of
    SigSD ->
      NodePR ValueSigNd endIx0 [] False

    DefSD ->
      NodePR ValueDeclNd endIx0 [] False

    UnknownSD ->
      case mode0 of
        StrictPM ->
          SkipPR endIx0 [diagParse "PFR007" range0 "cannot distinguish between a value signature and a value definition"] True

        TolerantPM ->
          NodePR ValueDeclNd endIx0 [diagParse "PFR008" range0 "cannot distinguish between a value signature and a value definition; treating it as a value definition"] True

orderDiags :: SyntaxKind -> PhaseRoot -> Bool -> Range -> [Diag]
orderDiags kind0 phase0 moduleSeen0 range0 =
  case kind0 of
    ModuleDeclNd
      | moduleSeen0 ->
          [diagParse "PFR002" range0 "duplicate module declaration at top level"]
      | phase0 /= HeadPH ->
          [diagParse "PFR003" range0 "module declaration must appear before imports and declarations"]
      | otherwise ->
          []

    ImportDeclNd
      | phase0 == DeclPH ->
          [diagParse "PFR004" range0 "import declaration must appear before ordinary declarations"]
      | otherwise ->
          []

    _ ->
      []

advancePhase :: SyntaxKind -> AccParse -> AccParse
advancePhase kind0 acc0 =
  case kind0 of
    ModuleDeclNd ->
      acc0
        { phasePA =
            case phasePA acc0 of
              HeadPH -> ImportPH
              phase1 -> phase1
        , moduleSeenPA = True
        }

    ImportDeclNd ->
      acc0
        { phasePA =
            case phasePA acc0 of
              DeclPH -> DeclPH
              _ -> ImportPH
        }

    _ ->
      acc0 { phasePA = DeclPH }

pushEvent :: ParseEvent -> AccParse -> AccParse
pushEvent ev0 acc0 = acc0 { eventsRevPA = ev0 : eventsRevPA acc0 }

pushDiag :: Diag -> AccParse -> AccParse
pushDiag diag0 acc0 =
  acc0
    { eventsRevPA = ErrorPE diag0 : eventsRevPA acc0
    , diagsRevPA = diag0 : diagsRevPA acc0
    }

pushDiags :: [Diag] -> AccParse -> AccParse
pushDiags diags0 acc0 = foldl' (\acc1 diag0 -> pushDiag diag0 acc1) acc0 diags0

emitNode :: SyntaxKind -> Int -> Int -> AccParse -> AccParse
emitNode kind0 startIx0 endIx0 acc0 =
  let acc1 = pushEvent (StartPE kind0) acc0
      acc2 = emitTokSpan startIx0 endIx0 acc1
  in
  pushEvent FinishPE acc2

emitTokSpan :: Int -> Int -> AccParse -> AccParse
emitTokSpan startIx0 endIx0 acc0 =
  foldl'
    (\acc1 ix0 -> pushEvent (TokPE (tokIxAt ix0)) acc1)
    acc0
    [startIx0 .. endIx0 - 1]

fallbackEvents :: Vector TokenLex -> Vector Diag -> Vector ParseEvent
fallbackEvents toks0 diags0 =
  let evs0 =
        [StartPE SourceFileNd]
          <> map ErrorPE (V.toList diags0)
          <> map (TokPE . tokIxAt) (liveTokIxs toks0)
          <> [FinishPE]
  in
  V.fromList evs0

liveTokIxs :: Vector TokenLex -> [Int]
liveTokIxs toks0 =
  [ ix0
  | ix0 <- [0 .. V.length toks0 - 1]
  , kindTokAt toks0 ix0 /= EndTk
  ]

findNodeEnd :: Vector TokenLex -> Int -> Int
findNodeEnd toks0 startIx0 = go (startIx0 + 1) depthZeroDS
  where
    go :: Int -> DepthScan -> Int
    go ix0 depth0
      | ix0 >= V.length toks0 = V.length toks0
      | otherwise =
          let kind0 = kindTokAt toks0 ix0
          in
          if isBoundaryKd depth0 kind0
            then ix0
            else go (ix0 + 1) (stepDepth kind0 depth0)

findSkipEnd :: ModeParse -> Vector TokenLex -> Int -> Int
findSkipEnd mode0 toks0 startIx0 =
  case mode0 of
    StrictPM -> min (startIx0 + 1) (V.length toks0)
    TolerantPM -> findNodeEnd toks0 startIx0

scanSigDef :: Vector TokenLex -> Int -> Int -> SigDef
scanSigDef toks0 startIx0 endIx0 = go (startIx0 + 1) depthZeroDS
  where
    go :: Int -> DepthScan -> SigDef
    go ix0 depth0
      | ix0 >= endIx0 = UnknownSD
      | otherwise =
          case kindTokAt toks0 ix0 of
            ColonTk | isFlatDepth depth0 -> SigSD
            EqualTk | isFlatDepth depth0 -> DefSD
            kind0 -> go (ix0 + 1) (stepDepth kind0 depth0)

nextSignificantKd :: Vector TokenLex -> Int -> Maybe SyntaxKind
nextSignificantKd toks0 startIx0 = go startIx0
  where
    go :: Int -> Maybe SyntaxKind
    go ix0
      | ix0 >= V.length toks0 = Nothing
      | otherwise =
          let kind0 = kindTokAt toks0 ix0
          in
          if kind0 == EndTk
            then Nothing
            else if isNeutralRootKd kind0
              then go (ix0 + 1)
              else Just kind0

rangeSlice :: Vector TokenLex -> Int -> Int -> Range
rangeSlice toks0 startIx0 endIx0
  | startIx0 >= endIx0 = emptyRange
  | otherwise =
      foldl'
        mergeRange
        (rangeTokAt toks0 startIx0)
        [ rangeTokAt toks0 ix0
        | ix0 <- [startIx0 + 1 .. endIx0 - 1]
        ]

tokIxAt :: Int -> TokIx
tokIxAt ix0 = TokIx (fromIntegral ix0)

kindTokAt :: Vector TokenLex -> Int -> SyntaxKind
kindTokAt toks0 ix0 = kindTL (toks0 V.! ix0)

rangeTokAt :: Vector TokenLex -> Int -> Range
rangeTokAt toks0 ix0 = rangeTL (toks0 V.! ix0)

isNeutralRootKd :: SyntaxKind -> Bool
isNeutralRootKd kind0 =
  isTriviaKd kind0
    || kind0 == LayoutOpenTk
    || kind0 == LayoutSepTk
    || kind0 == LayoutCloseTk

isRootStartKd :: SyntaxKind -> Bool
isRootStartKd kind0 =
  case kind0 of
    ModuleKwTk -> True
    ImportKwTk -> True
    TypeKwTk -> True
    RegionKwTk -> True
    AnchorKwTk -> True
    CellKwTk -> True
    NativeKwTk -> True
    ForeignKwTk -> True
    ThreadKwTk -> True
    InfixKwTk -> True
    LowerNameTk -> True
    _ -> False

isBoundaryKd :: DepthScan -> SyntaxKind -> Bool
isBoundaryKd depth0 kind0 =
  isFlatDepth depth0
    && (kind0 == EndTk
          || kind0 == LayoutSepTk
          || kind0 == LayoutCloseTk
          || isRootStartKd kind0)

depthZeroDS :: DepthScan
depthZeroDS = DepthScan 0 0 0 0

isFlatDepth :: DepthScan -> Bool
isFlatDepth depth0 =
  parenDP depth0 == 0
    && bracketDP depth0 == 0
    && braceDP depth0 == 0
    && layoutDP depth0 == 0

stepDepth :: SyntaxKind -> DepthScan -> DepthScan
stepDepth kind0 depth0 =
  case kind0 of
    LParenTk ->
      depth0 { parenDP = parenDP depth0 + 1 }

    RParenTk ->
      depth0 { parenDP = decNat (parenDP depth0) }

    LBracketTk ->
      depth0 { bracketDP = bracketDP depth0 + 1 }

    RBracketTk ->
      depth0 { bracketDP = decNat (bracketDP depth0) }

    LBraceTk ->
      depth0 { braceDP = braceDP depth0 + 1 }

    RBraceTk ->
      depth0 { braceDP = decNat (braceDP depth0) }

    LayoutOpenTk ->
      depth0 { layoutDP = layoutDP depth0 + 1 }

    LayoutCloseTk ->
      depth0 { layoutDP = decNat (layoutDP depth0) }

    _ ->
      depth0

decNat :: Int -> Int
decNat n0
  | n0 <= 0 = 0
  | otherwise = n0 - 1

diagParse :: Text -> Range -> Text -> Diag
diagParse code0 range0 msg0 = mkDiag (CodeDiag code0) ParseDG ErrorDS range0 msg0

diagEventErr :: EventErr -> Diag
diagEventErr err0 = mkDiag (CodeDiag "PFR900") ParseDG ErrorDS emptyRange (msgEventErr err0)

msgEventErr :: EventErr -> Text
msgEventErr err0 =
  case err0 of
    RootMissingEE ->
      "parser validation failed: missing root node"

    RootExtraEE ix0 kind0 ->
      "parser validation failed at event " <> tShow ix0 <> ": extra root node " <> tShow kind0

    StartNodeExpectedEE ix0 kind0 ->
      "parser validation failed at event " <> tShow ix0 <> ": expected a node kind, got " <> tShow kind0

    SyntheticTokenExpectedEE ix0 kind0 ->
      "parser validation failed at event " <> tShow ix0 <> ": expected a token kind for synthetic token emission, got " <> tShow kind0

    TokOutsideNodeEE ix0 tokIx0 ->
      "parser validation failed at event " <> tShow ix0 <> ": token reference outside any open node: " <> tShow tokIx0

    SyntheticOutsideNodeEE ix0 kind0 ->
      "parser validation failed at event " <> tShow ix0 <> ": synthetic token outside any open node: " <> tShow kind0

    TokNegativeEE ix0 tokIx0 ->
      "parser validation failed at event " <> tShow ix0 <> ": negative token reference: " <> tShow tokIx0

    TokOrderEE ix0 prevIx0 nextIx0 ->
      "parser validation failed at event " <> tShow ix0 <> ": token order regression from " <> tShow prevIx0 <> " to " <> tShow nextIx0

    FinishUnderflowEE ix0 ->
      "parser validation failed at event " <> tShow ix0 <> ": finish without matching start"

    NodeUnclosedEE kind0 ->
      "parser validation failed: unclosed node " <> tShow kind0

tShow :: Show a => a -> Text
tShow = T.pack . show