{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.Parse.Internal.Recover
  ( AnchorRecover(..)
  , recoverUntil
  , withRecover
  , topAnchors
  , exprAnchors
  , declAnchors
  ) where

import Fuddle.Compiler.Parse.Internal.State (Parser, anchorPop, anchorPush, bumpTok, peekTokMay)
import Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  , isTokenKd
  )
import Fuddle.Compiler.Syntax.Token (TokenLex(..))

data AnchorRecover =
    KindAR !SyntaxKind
  | TokenAR !SyntaxKind
  deriving stock (Eq, Ord, Show)

getSyntaxKind :: AnchorRecover -> SyntaxKind
getSyntaxKind (KindAR kd) = kd
getSyntaxKind (TokenAR kd) = kd

topAnchors :: [AnchorRecover]
topAnchors =
  [ TokenAR EndTk
  , TokenAR LayoutSepTk
  , TokenAR LayoutCloseTk
  , TokenAR DocLineTk
  , TokenAR DocBlockTk
  , TokenAR AtTk
  , KindAR ModuleDeclNd
  , KindAR ImportDeclNd
  , KindAR ValueSigNd
  , KindAR ValueDeclNd
  , KindAR TypeDeclNd
  , KindAR AliasDeclNd
  , KindAR EffectDeclNd
  , KindAR ForeignDeclNd
  , KindAR FixityDeclNd
  , KindAR RegionDeclNd
  , KindAR AnchorDeclNd
  , KindAR CellDeclNd
  , KindAR NativeDeclNd
  , KindAR ThreadSigNd
  , KindAR ThreadDeclNd
  ]

declAnchors :: [AnchorRecover]
declAnchors =
  [ TokenAR EndTk
  , TokenAR LayoutSepTk
  , TokenAR LayoutCloseTk
  , TokenAR InKwTk
  , TokenAR WhereKwTk
  , TokenAR DocLineTk
  , TokenAR DocBlockTk
  , TokenAR AtTk
  , KindAR ValueSigNd
  , KindAR ValueDeclNd
  , KindAR TypeDeclNd
  , KindAR AliasDeclNd
  , KindAR EffectDeclNd
  , KindAR ForeignDeclNd
  , KindAR FixityDeclNd
  , KindAR RegionDeclNd
  , KindAR AnchorDeclNd
  , KindAR CellDeclNd
  , KindAR NativeDeclNd
  , KindAR ThreadSigNd
  , KindAR ThreadDeclNd
  ]

exprAnchors :: [AnchorRecover]
exprAnchors =
  declAnchors <>
    [ TokenAR CommaTk
    , TokenAR PipeTk
    , TokenAR RParenTk
    , TokenAR RBracketTk
    , TokenAR RBraceTk
    , TokenAR ThenKwTk
    , TokenAR ElseKwTk
    , TokenAR OfKwTk
    , TokenAR CatchKwTk
    , TokenAR LtSlashTk
    , TokenAR SlashGtTk
    ]

withRecover :: [AnchorRecover] -> Parser a -> Parser a
withRecover aRecover action = do
  mapM_ (anchorPush . getSyntaxKind) aRecover
  res <- action
  anchorPop
  pure res

recoverUntil :: [AnchorRecover] -> Parser ()
recoverUntil ars = go
  where
    go = do
      tokMay <- peekTokMay
      case tokMay of
        Nothing -> pure ()
        Just (_, tok)
          | stopTok ars (kindTL tok) -> pure ()
          | otherwise -> do
              _ <- bumpTok
              go

stopTok :: [AnchorRecover] -> SyntaxKind -> Bool
stopTok ars tk = tk == EndTk || any (matchAR tk) ars

matchAR :: SyntaxKind -> AnchorRecover -> Bool
matchAR tk ar =
  case ar of
    TokenAR kd -> tk == kd
    KindAR kd -> startKd kd tk

startKd :: SyntaxKind -> SyntaxKind -> Bool
startKd kd tk
  | isTokenKd kd = kd == tk
  | otherwise =
      case kd of
        SourceFileNd -> tk == EndTk || topStartTk tk
        ModuleDeclNd -> tk == ModuleKwTk
        ModuleHeadNd -> tk == ModuleKwTk
        ExportListNd -> tk == LParenTk
        ExportItemNd -> nameStartTk tk
        ImportDeclNd -> tk == ImportKwTk
        ImportAliasNd -> tk == AsKwTk
        ImportItemsNd -> tk == ExposingKwTk
        ImportItemNd -> nameStartTk tk

        DocBlockNd -> tk == DocLineTk || tk == DocBlockTk
        AttrNd -> tk == AtTk
        AttrListNd -> tk == AtTk
        PragmaNd -> tk == AtTk

        ValueSigNd -> tk == LowerNameTk
        ValueDeclNd -> tk == LowerNameTk
        TypeDeclNd -> tk == TypeKwTk
        CtorDeclNd -> tk == UpperNameTk
        AliasDeclNd -> tk == TypeKwTk
        EffectDeclNd -> tk == EffectKwTk
        ForeignDeclNd -> tk == ForeignKwTk
        FixityDeclNd -> tk == InfixKwTk
        RegionDeclNd -> tk == RegionKwTk
        AnchorDeclNd -> tk == AnchorKwTk
        CellDeclNd -> tk == CellKwTk
        NativeDeclNd -> tk == NativeKwTk
        ThreadSigNd -> tk == ThreadKwTk
        ThreadDeclNd -> tk == ThreadKwTk

        TypeAnnNd -> typeStartTk tk
        TypeVarNd -> tk == LowerNameTk
        TypeCtorNd -> tk == UpperNameTk
        TypeApplyNd -> typeAtomStartTk tk
        TypeTupleNd -> tk == LParenTk
        TypeRecordNd -> tk == LBraceTk
        TypeFieldNd -> tk == LowerNameTk
        TypeRowNd -> tk == LBraceTk
        TypeArrowNd -> typeStartTk tk
        TypeParenNd -> tk == LParenTk
        TypeUnitNd -> tk == LParenTk
        TypeEffectNd -> typeStartTk tk
        RowEffectNd -> tk == LtTk
        ItemEffectNd -> tk == LowerNameTk
        FailEffectNd -> tk == LowerNameTk
        TypeAnchorNd -> tk == UpperNameTk

        WildcardPatternNd -> tk == UnderscoreTk
        VarPatternNd -> tk == LowerNameTk
        CtorPatternNd -> tk == UpperNameTk
        LiteralPatternNd -> literalStartTk tk
        TuplePatternNd -> tk == LParenTk
        ListPatternNd -> tk == LBracketTk
        RecordPatternNd -> tk == LBraceTk
        ConsPatternNd -> patternStartTk tk
        AsPatternNd -> patternStartTk tk
        ParenPatternNd -> tk == LParenTk
        UnitPatternNd -> tk == LParenTk

        NameExprNd -> nameStartTk tk
        LiteralExprNd -> literalStartTk tk
        HoleExprNd -> tk == HoleNameTk
        ParenExprNd -> tk == LParenTk
        TupleExprNd -> tk == LParenTk
        ListExprNd -> tk == LBracketTk
        RecordExprNd -> tk == LBraceTk
        FieldRecordNd -> tk == LowerNameTk
        UpdateRecordExprNd -> tk == LBraceTk
        AccessorExprNd -> tk == DotTk
        AccessExprNd -> primaryStartTk tk
        UnitExprNd -> tk == LParenTk
        LambdaExprNd -> tk == BackslashTk
        ApplyExprNd -> primaryStartTk tk
        BinaryExprNd -> exprStartTk tk
        IfExprNd -> tk == IfKwTk
        CaseExprNd -> tk == CaseKwTk
        BranchCaseNd -> patternStartTk tk
        LetExprNd -> tk == LetKwTk
        BindingLetNd -> tk == LowerNameTk
        DoExprNd -> tk == DoKwTk
        BindDoNd -> patternStartTk tk
        LetDoNd -> tk == LetKwTk
        StmtDoNd -> exprStartTk tk
        ClauseCatchNd -> tk == CatchKwTk
        ArmCatchNd -> patternStartTk tk
        AnchorExprNd -> anchorHeadStartTk tk

        ExprMarkupNd -> anchorHeadStartTk tk
        ElemMarkupNd -> anchorHeadStartTk tk
        AttrMarkupNd -> exprStartTk tk
        TextMarkupNd -> literalStartTk tk
        InterpMarkupNd -> tk == InterpOpenTk
        FragmentMarkupNd -> tk == LBracketTk

        BlockForeignNd -> tk == ForeignKwTk
        ExprForeignNd -> tk == ForeignKwTk

        MissingNd -> False
        ErrorNd -> False
        SkippedNd -> False
        UnknownKd -> False

        EndTk -> False
        WhitespaceTk -> False
        NewlineTk -> False
        CommentLineTk -> False
        CommentBlockTk -> False
        DocLineTk -> False
        DocBlockTk -> False
        LayoutOpenTk -> False
        LayoutSepTk -> False
        LayoutCloseTk -> False
        LowerNameTk -> False
        UpperNameTk -> False
        HoleNameTk -> False
        OperatorTk -> False
        IntTk -> False
        FloatTk -> False
        CharTk -> False
        StringOpenTk -> False
        StringChunkTk -> False
        StringCloseTk -> False
        MultiStringOpenTk -> False
        MultiStringChunkTk -> False
        MultiStringCloseTk -> False
        InterpOpenTk -> False
        InterpCloseTk -> False
        ModuleKwTk -> False
        ImportKwTk -> False
        ExposingKwTk -> False
        AsKwTk -> False
        TypeKwTk -> False
        AliasKwTk -> False
        EffectKwTk -> False
        ForeignKwTk -> False
        IfKwTk -> False
        ThenKwTk -> False
        ElseKwTk -> False
        CaseKwTk -> False
        OfKwTk -> False
        LetKwTk -> False
        InKwTk -> False
        DoKwTk -> False
        CatchKwTk -> False
        WhereKwTk -> False
        InfixKwTk -> False
        LeftKwTk -> False
        RightKwTk -> False
        RegionKwTk -> False
        AnchorKwTk -> False
        CellKwTk -> False
        NativeKwTk -> False
        ThreadKwTk -> False
        LParenTk -> False
        RParenTk -> False
        LBracketTk -> False
        RBracketTk -> False
        LBraceTk -> False
        RBraceTk -> False
        LtTk -> False
        GtTk -> False
        LtSlashTk -> False
        SlashGtTk -> False
        CommaTk -> False
        ColonTk -> False
        ColonColonTk -> False
        SemiTk -> False
        DotTk -> False
        DotDotTk -> False
        PipeTk -> False
        EqualTk -> False
        BangTk -> False
        BackslashTk -> False
        ArrowThinTk -> False
        ArrowFatTk -> False
        ArrowLeftTk -> False
        AtTk -> False
        QuestionTk -> False
        UnderscoreTk -> False

nameStartTk :: SyntaxKind -> Bool
nameStartTk tk =
  case tk of
    LowerNameTk -> True
    UpperNameTk -> True
    _ -> False

literalStartTk :: SyntaxKind -> Bool
literalStartTk tk =
  case tk of
    IntTk -> True
    FloatTk -> True
    CharTk -> True
    StringOpenTk -> True
    MultiStringOpenTk -> True
    _ -> False

typeAtomStartTk :: SyntaxKind -> Bool
typeAtomStartTk tk =
  case tk of
    LowerNameTk -> True
    UpperNameTk -> True
    LParenTk -> True
    LBraceTk -> True
    _ -> False

typeStartTk :: SyntaxKind -> Bool
typeStartTk = typeAtomStartTk

patternStartTk :: SyntaxKind -> Bool
patternStartTk tk =
  case tk of
    UnderscoreTk -> True
    LowerNameTk -> True
    UpperNameTk -> True
    IntTk -> True
    FloatTk -> True
    CharTk -> True
    StringOpenTk -> True
    MultiStringOpenTk -> True
    LParenTk -> True
    LBracketTk -> True
    LBraceTk -> True
    _ -> False

anchorHeadStartTk :: SyntaxKind -> Bool
anchorHeadStartTk tk =
  case tk of
    LowerNameTk -> True
    LParenTk -> True
    _ -> False

primaryStartTk :: SyntaxKind -> Bool
primaryStartTk tk =
  case tk of
    LowerNameTk -> True
    UpperNameTk -> True
    HoleNameTk -> True
    DotTk -> True
    LParenTk -> True
    LBracketTk -> True
    LBraceTk -> True
    IntTk -> True
    FloatTk -> True
    CharTk -> True
    StringOpenTk -> True
    MultiStringOpenTk -> True
    _ -> False

exprStartTk :: SyntaxKind -> Bool
exprStartTk tk =
  primaryStartTk tk ||
  case tk of
    IfKwTk -> True
    CaseKwTk -> True
    LetKwTk -> True
    DoKwTk -> True
    BackslashTk -> True
    _ -> False

declStartTk :: SyntaxKind -> Bool
declStartTk tk =
  case tk of
    LowerNameTk -> True
    TypeKwTk -> True
    EffectKwTk -> True
    ForeignKwTk -> True
    InfixKwTk -> True
    RegionKwTk -> True
    AnchorKwTk -> True
    CellKwTk -> True
    NativeKwTk -> True
    ThreadKwTk -> True
    _ -> False

topStartTk :: SyntaxKind -> Bool
topStartTk tk =
  declStartTk tk ||
  case tk of
    ModuleKwTk -> True
    ImportKwTk -> True
    DocLineTk -> True
    DocBlockTk -> True
    AtTk -> True
    _ -> False