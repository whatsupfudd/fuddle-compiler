{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Syntax.Kind
  ( SyntaxKind(..)
  , kindTag
  , isTriviaKd
  , isTokenKd
  , isNodeKd
  , isRecoveryKd
  ) where

import Data.Word (Word16)

-- Invariant:
--   * all token kinds come before all node kinds
--   * all recovery node kinds stay last
-- The classification helpers rely on this ordering, so new constructors must be
-- inserted in the right section.

data SyntaxKind =
    UnknownKd

  -- raw / trivia / synthetic tokens
  | EndTk
  | WhitespaceTk
  | NewlineTk
  | CommentLineTk
  | CommentBlockTk
  | DocLineTk
  | DocBlockTk
  | LayoutOpenTk
  | LayoutSepTk
  | LayoutCloseTk

  -- identifiers / operators / literals
  | LowerNameTk
  | UpperNameTk
  | HoleNameTk
  | OperatorTk
  | IntTk
  | FloatTk
  | CharTk
  | StringOpenTk
  | StringChunkTk
  | StringCloseTk
  | MultiStringOpenTk
  | MultiStringChunkTk
  | MultiStringCloseTk
  | InterpOpenTk
  | InterpCloseTk

  -- keywords
  | ModuleKwTk
  | ImportKwTk
  | ExposingKwTk
  | AsKwTk
  | TypeKwTk
  | AliasKwTk
  | EffectKwTk
  | ForeignKwTk
  | IfKwTk
  | ThenKwTk
  | ElseKwTk
  | CaseKwTk
  | OfKwTk
  | LetKwTk
  | InKwTk
  | DoKwTk
  | CatchKwTk
  | WhereKwTk
  | InfixKwTk
  | LeftKwTk
  | RightKwTk
  | RegionKwTk
  | AnchorKwTk
  | CellKwTk
  | NativeKwTk
  | ThreadKwTk

  -- punctuation / fixed symbols
  | LParenTk
  | RParenTk
  | LBracketTk
  | RBracketTk
  | LBraceTk
  | RBraceTk
  | LtTk
  | GtTk
  | LtSlashTk
  | SlashGtTk
  | CommaTk
  | ColonTk
  | ColonColonTk
  | SemiTk
  | DotTk
  | DotDotTk
  | PipeTk
  | EqualTk
  | BangTk
  | BackslashTk
  | ArrowThinTk
  | ArrowFatTk
  | ArrowLeftTk
  | AtTk
  | QuestionTk
  | UnderscoreTk

  -- file / module surface
  | SourceFileNd
  | ModuleDeclNd
  | ModuleHeadNd
  | ExportListNd
  | ExportItemNd
  | ImportDeclNd
  | ImportAliasNd
  | ImportItemsNd
  | ImportItemNd

  -- documentation / attributes / pragmas
  | DocBlockNd
  | AttrNd
  | AttrListNd
  | PragmaNd

  -- top-level declarations
  | ValueSigNd
  | ValueDeclNd
  | TypeDeclNd
  | CtorDeclNd
  | AliasDeclNd
  | EffectDeclNd
  | ForeignDeclNd
  | FixityDeclNd
  | RegionDeclNd
  | AnchorDeclNd
  | CellDeclNd
  | NativeDeclNd
  | ThreadSigNd
  | ThreadDeclNd

  -- type syntax
  | TypeAnnNd
  | TypeVarNd
  | TypeCtorNd
  | TypeApplyNd
  | TypeTupleNd
  | TypeRecordNd
  | TypeFieldNd
  | TypeRowNd
  | TypeArrowNd
  | TypeParenNd
  | TypeUnitNd
  | TypeEffectNd
  | RowEffectNd
  | ItemEffectNd
  | FailEffectNd
  | TypeAnchorNd

  -- pattern syntax
  | WildcardPatternNd
  | VarPatternNd
  | CtorPatternNd
  | LiteralPatternNd
  | TuplePatternNd
  | ListPatternNd
  | RecordPatternNd
  | ConsPatternNd
  | AsPatternNd
  | ParenPatternNd
  | UnitPatternNd

  -- expression syntax
  | NameExprNd
  | LiteralExprNd
  | HoleExprNd
  | ParenExprNd
  | TupleExprNd
  | ListExprNd
  | RecordExprNd
  | FieldRecordNd
  | UpdateRecordExprNd
  | AccessorExprNd
  | AccessExprNd
  | UnitExprNd
  | LambdaExprNd
  | ApplyExprNd
  | BinaryExprNd
  | IfExprNd
  | CaseExprNd
  | BranchCaseNd
  | LetExprNd
  | BindingLetNd
  | DoExprNd
  | BindDoNd
  | LetDoNd
  | StmtDoNd
  | ClauseCatchNd
  | ArmCatchNd
  | AnchorExprNd

  -- markup / template syntax
  | ExprMarkupNd
  | ElemMarkupNd
  | AttrMarkupNd
  | TextMarkupNd
  | InterpMarkupNd
  | FragmentMarkupNd

  -- foreign / embedded syntax
  | BlockForeignNd
  | ExprForeignNd

  -- recovery / tolerant parsing
  | MissingNd
  | ErrorNd
  | SkippedNd
  deriving stock (Eq, Ord, Show, Enum, Bounded)

kindTag :: SyntaxKind -> Word16
kindTag = fromIntegral . fromEnum

isTriviaKd :: SyntaxKind -> Bool
isTriviaKd kd =
  case kd of
    WhitespaceTk -> True
    NewlineTk -> True
    CommentLineTk -> True
    CommentBlockTk -> True
    DocLineTk -> True
    DocBlockTk -> True
    _ -> False

isTokenKd :: SyntaxKind -> Bool
isTokenKd kd = kd < firstNodeKd

isNodeKd :: SyntaxKind -> Bool
isNodeKd kd = kd >= firstNodeKd

isRecoveryKd :: SyntaxKind -> Bool
isRecoveryKd kd = kd >= firstRecoveryKd

firstNodeKd :: SyntaxKind
firstNodeKd = SourceFileNd

firstRecoveryKd :: SyntaxKind
firstRecoveryKd = MissingNd