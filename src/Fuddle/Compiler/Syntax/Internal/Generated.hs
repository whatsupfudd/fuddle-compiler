module Fuddle.Compiler.Syntax.Internal.Generated
  ( SourceFile(..)
  , ModuleDecl(..)
  , ModuleHead(..)
  , ExportList(..)
  , ExportItem(..)
  , ImportDecl(..)
  , ImportAlias(..)
  , ImportItems(..)
  , ImportItem(..)
  , DocBlock(..)
  , Attr(..)
  , AttrList(..)
  , Pragma(..)
  , ValueDecl(..)
  , TypeDecl(..)
  , AliasDecl(..)
  , EffectDecl(..)
  , ForeignDecl(..)
  , FixityDecl(..)
  , TypeAnn(..)
  , TypeVar(..)
  , TypeCtor(..)
  , TypeApply(..)
  , TypeTuple(..)
  , TypeRecord(..)
  , TypeField(..)
  , TypeRow(..)
  , TypeArrow(..)
  , TypeParen(..)
  , TypeEffect(..)
  , WildcardPattern(..)
  , VarPattern(..)
  , CtorPattern(..)
  , LiteralPattern(..)
  , TuplePattern(..)
  , ListPattern(..)
  , RecordPattern(..)
  , ConsPattern(..)
  , AsPattern(..)
  , ParenPattern(..)
  , NameExpr(..)
  , LiteralExpr(..)
  , HoleExpr(..)
  , ParenExpr(..)
  , TupleExpr(..)
  , ListExpr(..)
  , RecordExpr(..)
  , FieldRecord(..)
  , UpdateRecordExpr(..)
  , LambdaExpr(..)
  , ApplyExpr(..)
  , BinaryExpr(..)
  , IfExpr(..)
  , CaseExpr(..)
  , BranchCase(..)
  , LetExpr(..)
  , BindingLet(..)
  , DoExpr(..)
  , StmtDo(..)
  , CatchDo(..)
  , AccessExpr(..)
  , ExprMarkup(..)
  , ElemMarkup(..)
  , AttrMarkup(..)
  , TextMarkup(..)
  , InterpMarkup(..)
  , FragmentMarkup(..)
  , BlockForeign(..)
  , ExprForeign(..)
  , MissingNode(..)
  , ErrorNode(..)
  , SkippedNode(..)
  , TopDecl(..)
  , TypeSyn(..)
  , PatternSyn(..)
  , ExprSyn(..)
  , MarkupSyn(..)
  , RecoverSyn(..)
  , sourceFileMay
  , kindAst
  , rangeAst
  , textAst
  , parentNodeMayAst
  , parentAstMay
  , ancestorNodesAst
  , ancestorAsts
  , childAstMay
  , childAsts
  , childAstMayOf
  , childAstsOf
  , nodeKdMay
  , nodesKd
  , tokenKdMay
  , tokensKd
  , castTopDecl
  , castTypeSyn
  , castPatternSyn
  , castExprSyn
  , castMarkupSyn
  , castRecoverSyn
  , moduleDeclMay
  , importDecls
  , topDecls
  , moduleHeadMay
  , exportListMay
  , exportListModuleMay
  , exportItems
  , importAliasMay
  , importItemsMay
  , importItems
  , typeFields
  , recordFields
  , caseBranches
  , letBindings
  , doStmts
  , doCatchMay
  , markupChildren
  ) where

import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fuddle.Compiler.Base.Range (Range)
import Fuddle.Compiler.Syntax.AstClass (AstNode(..))
import Fuddle.Compiler.Syntax.Kind (SyntaxKind(..))
import Fuddle.Compiler.Syntax.Red (NodeSyntax, TokenSyntax, TreeSyntax)
import qualified Fuddle.Compiler.Syntax.Red as R

newtype SourceFile = SourceFile { nodeA :: NodeSyntax }
newtype ModuleDecl = ModuleDecl { nodeA :: NodeSyntax }
newtype ModuleHead = ModuleHead { nodeA :: NodeSyntax }
newtype ExportList = ExportList { nodeA :: NodeSyntax }
newtype ExportItem = ExportItem { nodeA :: NodeSyntax }
newtype ImportDecl = ImportDecl { nodeA :: NodeSyntax }
newtype ImportAlias = ImportAlias { nodeA :: NodeSyntax }
newtype ImportItems = ImportItems { nodeA :: NodeSyntax }
newtype ImportItem = ImportItem { nodeA :: NodeSyntax }
newtype DocBlock = DocBlock { nodeA :: NodeSyntax }
newtype Attr = Attr { nodeA :: NodeSyntax }
newtype AttrList = AttrList { nodeA :: NodeSyntax }
newtype Pragma = Pragma { nodeA :: NodeSyntax }
newtype ValueDecl = ValueDecl { nodeA :: NodeSyntax }
newtype TypeDecl = TypeDecl { nodeA :: NodeSyntax }
newtype AliasDecl = AliasDecl { nodeA :: NodeSyntax }
newtype EffectDecl = EffectDecl { nodeA :: NodeSyntax }
newtype ForeignDecl = ForeignDecl { nodeA :: NodeSyntax }
newtype FixityDecl = FixityDecl { nodeA :: NodeSyntax }
newtype TypeAnn = TypeAnn { nodeA :: NodeSyntax }
newtype TypeVar = TypeVar { nodeA :: NodeSyntax }
newtype TypeCtor = TypeCtor { nodeA :: NodeSyntax }
newtype TypeApply = TypeApply { nodeA :: NodeSyntax }
newtype TypeTuple = TypeTuple { nodeA :: NodeSyntax }
newtype TypeRecord = TypeRecord { nodeA :: NodeSyntax }
newtype TypeField = TypeField { nodeA :: NodeSyntax }
newtype TypeRow = TypeRow { nodeA :: NodeSyntax }
newtype TypeArrow = TypeArrow { nodeA :: NodeSyntax }
newtype TypeParen = TypeParen { nodeA :: NodeSyntax }
newtype TypeEffect = TypeEffect { nodeA :: NodeSyntax }
newtype WildcardPattern = WildcardPattern { nodeA :: NodeSyntax }
newtype VarPattern = VarPattern { nodeA :: NodeSyntax }
newtype CtorPattern = CtorPattern { nodeA :: NodeSyntax }
newtype LiteralPattern = LiteralPattern { nodeA :: NodeSyntax }
newtype TuplePattern = TuplePattern { nodeA :: NodeSyntax }
newtype ListPattern = ListPattern { nodeA :: NodeSyntax }
newtype RecordPattern = RecordPattern { nodeA :: NodeSyntax }
newtype ConsPattern = ConsPattern { nodeA :: NodeSyntax }
newtype AsPattern = AsPattern { nodeA :: NodeSyntax }
newtype ParenPattern = ParenPattern { nodeA :: NodeSyntax }
newtype NameExpr = NameExpr { nodeA :: NodeSyntax }
newtype LiteralExpr = LiteralExpr { nodeA :: NodeSyntax }
newtype HoleExpr = HoleExpr { nodeA :: NodeSyntax }
newtype ParenExpr = ParenExpr { nodeA :: NodeSyntax }
newtype TupleExpr = TupleExpr { nodeA :: NodeSyntax }
newtype ListExpr = ListExpr { nodeA :: NodeSyntax }
newtype RecordExpr = RecordExpr { nodeA :: NodeSyntax }
newtype FieldRecord = FieldRecord { nodeA :: NodeSyntax }
newtype UpdateRecordExpr = UpdateRecordExpr { nodeA :: NodeSyntax }
newtype LambdaExpr = LambdaExpr { nodeA :: NodeSyntax }
newtype ApplyExpr = ApplyExpr { nodeA :: NodeSyntax }
newtype BinaryExpr = BinaryExpr { nodeA :: NodeSyntax }
newtype IfExpr = IfExpr { nodeA :: NodeSyntax }
newtype CaseExpr = CaseExpr { nodeA :: NodeSyntax }
newtype BranchCase = BranchCase { nodeA :: NodeSyntax }
newtype LetExpr = LetExpr { nodeA :: NodeSyntax }
newtype BindingLet = BindingLet { nodeA :: NodeSyntax }
newtype DoExpr = DoExpr { nodeA :: NodeSyntax }
newtype StmtDo = StmtDo { nodeA :: NodeSyntax }
newtype CatchDo = CatchDo { nodeA :: NodeSyntax }
newtype AccessExpr = AccessExpr { nodeA :: NodeSyntax }
newtype ExprMarkup = ExprMarkup { nodeA :: NodeSyntax }
newtype ElemMarkup = ElemMarkup { nodeA :: NodeSyntax }
newtype AttrMarkup = AttrMarkup { nodeA :: NodeSyntax }
newtype TextMarkup = TextMarkup { nodeA :: NodeSyntax }
newtype InterpMarkup = InterpMarkup { nodeA :: NodeSyntax }
newtype FragmentMarkup = FragmentMarkup { nodeA :: NodeSyntax }
newtype BlockForeign = BlockForeign { nodeA :: NodeSyntax }
newtype ExprForeign = ExprForeign { nodeA :: NodeSyntax }
newtype MissingNode = MissingNode { nodeA :: NodeSyntax }
newtype ErrorNode = ErrorNode { nodeA :: NodeSyntax }
newtype SkippedNode = SkippedNode { nodeA :: NodeSyntax }

data TopDecl
  = ValueTD ValueDecl
  | TypeTD TypeDecl
  | AliasTD AliasDecl
  | EffectTD EffectDecl
  | ForeignTD ForeignDecl
  | FixityTD FixityDecl

data TypeSyn
  = AnnTS TypeAnn
  | VarTS TypeVar
  | CtorTS TypeCtor
  | ApplyTS TypeApply
  | TupleTS TypeTuple
  | RecordTS TypeRecord
  | FieldTS TypeField
  | RowTS TypeRow
  | ArrowTS TypeArrow
  | ParenTS TypeParen
  | EffectTS TypeEffect

data PatternSyn
  = WildcardPS WildcardPattern
  | VarPS VarPattern
  | CtorPS CtorPattern
  | LiteralPS LiteralPattern
  | TuplePS TuplePattern
  | ListPS ListPattern
  | RecordPS RecordPattern
  | ConsPS ConsPattern
  | AsPS AsPattern
  | ParenPS ParenPattern

data ExprSyn
  = NameES NameExpr
  | LiteralES LiteralExpr
  | HoleES HoleExpr
  | ParenES ParenExpr
  | TupleES TupleExpr
  | ListES ListExpr
  | RecordES RecordExpr
  | UpdateRecordES UpdateRecordExpr
  | LambdaES LambdaExpr
  | ApplyES ApplyExpr
  | BinaryES BinaryExpr
  | IfES IfExpr
  | CaseES CaseExpr
  | LetES LetExpr
  | DoES DoExpr
  | AccessES AccessExpr
  | MarkupES ExprMarkup
  | ForeignES ExprForeign

data MarkupSyn
  = ExprMS ExprMarkup
  | ElemMS ElemMarkup
  | AttrMS AttrMarkup
  | TextMS TextMarkup
  | InterpMS InterpMarkup
  | FragmentMS FragmentMarkup

data RecoverSyn
  = MissingRS MissingNode
  | ErrorRS ErrorNode
  | SkippedRS SkippedNode

sourceFileMay :: TreeSyntax -> Maybe SourceFile
sourceFileMay tree = castNode (R.rootNode tree)

kindAst :: AstNode a => a -> SyntaxKind
kindAst ast = R.kindNode (nodeAst ast)

rangeAst :: AstNode a => a -> Range
rangeAst ast = R.rangeNode (nodeAst ast)

textAst :: AstNode a => a -> Text
textAst ast = R.textNode (nodeAst ast)

parentNodeMayAst :: AstNode a => a -> Maybe NodeSyntax
parentNodeMayAst ast = R.parentNodeMay (nodeAst ast)

parentAstMay :: (AstNode a, AstNode b) => b -> Maybe a
parentAstMay ast = R.parentNodeMay (nodeAst ast) >>= castNode

ancestorNodesAst :: AstNode a => a -> Vector NodeSyntax
ancestorNodesAst ast = R.ancestorsNode (nodeAst ast)

ancestorAsts :: (AstNode a, AstNode b) => b -> Vector a
ancestorAsts ast = V.mapMaybe castNode (R.ancestorsNode (nodeAst ast))

childAstMay :: AstNode a => NodeSyntax -> Maybe a
childAstMay nd = childAsts nd V.!? 0

childAsts :: AstNode a => NodeSyntax -> Vector a
childAsts nd = V.mapMaybe castNode (R.childNodes nd)

childAstMayOf :: (AstNode a, AstNode b) => b -> Maybe a
childAstMayOf ast = childAstMay (nodeAst ast)

childAstsOf :: (AstNode a, AstNode b) => b -> Vector a
childAstsOf ast = childAsts (nodeAst ast)

nodeKdMay :: SyntaxKind -> NodeSyntax -> Maybe NodeSyntax
nodeKdMay kd nd = find ((== kd) . R.kindNode) (R.childNodes nd)

nodesKd :: SyntaxKind -> NodeSyntax -> Vector NodeSyntax
nodesKd kd nd = V.filter ((== kd) . R.kindNode) (R.childNodes nd)

tokenKdMay :: SyntaxKind -> NodeSyntax -> Maybe TokenSyntax
tokenKdMay kd nd = find ((== kd) . R.kindToken) (R.childTokens nd)

tokensKd :: SyntaxKind -> NodeSyntax -> Vector TokenSyntax
tokensKd kd nd = V.filter ((== kd) . R.kindToken) (R.childTokens nd)

castTopDecl :: NodeSyntax -> Maybe TopDecl
castTopDecl nd =
  case R.kindNode nd of
    ValueDeclNd -> ValueTD <$> castNode nd
    TypeDeclNd -> TypeTD <$> castNode nd
    AliasDeclNd -> AliasTD <$> castNode nd
    EffectDeclNd -> EffectTD <$> castNode nd
    ForeignDeclNd -> ForeignTD <$> castNode nd
    FixityDeclNd -> FixityTD <$> castNode nd
    _ -> Nothing

castTypeSyn :: NodeSyntax -> Maybe TypeSyn
castTypeSyn nd =
  case R.kindNode nd of
    TypeAnnNd -> AnnTS <$> castNode nd
    TypeVarNd -> VarTS <$> castNode nd
    TypeCtorNd -> CtorTS <$> castNode nd
    TypeApplyNd -> ApplyTS <$> castNode nd
    TypeTupleNd -> TupleTS <$> castNode nd
    TypeRecordNd -> RecordTS <$> castNode nd
    TypeFieldNd -> FieldTS <$> castNode nd
    TypeRowNd -> RowTS <$> castNode nd
    TypeArrowNd -> ArrowTS <$> castNode nd
    TypeParenNd -> ParenTS <$> castNode nd
    TypeEffectNd -> EffectTS <$> castNode nd
    _ -> Nothing

castPatternSyn :: NodeSyntax -> Maybe PatternSyn
castPatternSyn nd =
  case R.kindNode nd of
    WildcardPatternNd -> WildcardPS <$> castNode nd
    VarPatternNd -> VarPS <$> castNode nd
    CtorPatternNd -> CtorPS <$> castNode nd
    LiteralPatternNd -> LiteralPS <$> castNode nd
    TuplePatternNd -> TuplePS <$> castNode nd
    ListPatternNd -> ListPS <$> castNode nd
    RecordPatternNd -> RecordPS <$> castNode nd
    ConsPatternNd -> ConsPS <$> castNode nd
    AsPatternNd -> AsPS <$> castNode nd
    ParenPatternNd -> ParenPS <$> castNode nd
    _ -> Nothing

castExprSyn :: NodeSyntax -> Maybe ExprSyn
castExprSyn nd =
  case R.kindNode nd of
    NameExprNd -> NameES <$> castNode nd
    LiteralExprNd -> LiteralES <$> castNode nd
    HoleExprNd -> HoleES <$> castNode nd
    ParenExprNd -> ParenES <$> castNode nd
    TupleExprNd -> TupleES <$> castNode nd
    ListExprNd -> ListES <$> castNode nd
    RecordExprNd -> RecordES <$> castNode nd
    UpdateRecordExprNd -> UpdateRecordES <$> castNode nd
    LambdaExprNd -> LambdaES <$> castNode nd
    ApplyExprNd -> ApplyES <$> castNode nd
    BinaryExprNd -> BinaryES <$> castNode nd
    IfExprNd -> IfES <$> castNode nd
    CaseExprNd -> CaseES <$> castNode nd
    LetExprNd -> LetES <$> castNode nd
    DoExprNd -> DoES <$> castNode nd
    AccessExprNd -> AccessES <$> castNode nd
    ExprMarkupNd -> MarkupES <$> castNode nd
    ExprForeignNd -> ForeignES <$> castNode nd
    _ -> Nothing

castMarkupSyn :: NodeSyntax -> Maybe MarkupSyn
castMarkupSyn nd =
  case R.kindNode nd of
    ExprMarkupNd -> ExprMS <$> castNode nd
    ElemMarkupNd -> ElemMS <$> castNode nd
    AttrMarkupNd -> AttrMS <$> castNode nd
    TextMarkupNd -> TextMS <$> castNode nd
    InterpMarkupNd -> InterpMS <$> castNode nd
    FragmentMarkupNd -> FragmentMS <$> castNode nd
    _ -> Nothing

castRecoverSyn :: NodeSyntax -> Maybe RecoverSyn
castRecoverSyn nd =
  case R.kindNode nd of
    MissingNd -> MissingRS <$> castNode nd
    ErrorNd -> ErrorRS <$> castNode nd
    SkippedNd -> SkippedRS <$> castNode nd
    _ -> Nothing

moduleDeclMay :: SourceFile -> Maybe ModuleDecl
moduleDeclMay src = childAstMayOf src

importDecls :: SourceFile -> Vector ImportDecl
importDecls = childAstsOf

topDecls :: SourceFile -> Vector TopDecl
topDecls src = V.mapMaybe castTopDecl (R.childNodes src.nodeA)

moduleHeadMay :: ModuleDecl -> Maybe ModuleHead
moduleHeadMay decl = childAstMayOf decl

exportListMay :: ModuleHead -> Maybe ExportList
exportListMay head0 = childAstMayOf head0

exportListModuleMay :: ModuleDecl -> Maybe ExportList
exportListModuleMay decl =
  case moduleHeadMay decl of
    Just head0 ->
      case exportListMay head0 of
        Just exports0 -> Just exports0
        Nothing -> childAstMayOf decl
    Nothing -> childAstMayOf decl

exportItems :: ExportList -> Vector ExportItem
exportItems = childAstsOf

importAliasMay :: ImportDecl -> Maybe ImportAlias
importAliasMay= childAstMayOf

importItemsMay :: ImportDecl -> Maybe ImportItems
importItemsMay = childAstMayOf

importItems :: ImportItems -> Vector ImportItem
importItems = childAstsOf

typeFields :: TypeRecord -> Vector TypeField
typeFields = childAstsOf

recordFields :: RecordExpr -> Vector FieldRecord
recordFields = childAstsOf

caseBranches :: CaseExpr -> Vector BranchCase
caseBranches = childAstsOf

letBindings :: LetExpr -> Vector BindingLet
letBindings = childAstsOf

doStmts :: DoExpr -> Vector StmtDo
doStmts = childAstsOf

doCatchMay :: DoExpr -> Maybe CatchDo
doCatchMay = childAstMayOf

markupChildren :: ExprMarkup -> Vector MarkupSyn
markupChildren expr0 = V.mapMaybe castMarkupSyn (R.childNodes expr0.nodeA)

castKd :: SyntaxKind -> (NodeSyntax -> a) -> NodeSyntax -> Maybe a
castKd kd wrap nd
  | R.kindNode nd == kd = Just (wrap nd)
  | otherwise = Nothing

instance AstNode SourceFile where
  canCastNode kd = kd == SourceFileNd
  castNode = castKd SourceFileNd SourceFile
  nodeAst ast = ast.nodeA

instance AstNode ModuleDecl where
  canCastNode kd = kd == ModuleDeclNd
  castNode = castKd ModuleDeclNd ModuleDecl
  nodeAst ast = ast.nodeA

instance AstNode ModuleHead where
  canCastNode kd = kd == ModuleHeadNd
  castNode = castKd ModuleHeadNd ModuleHead
  nodeAst ast = ast.nodeA

instance AstNode ExportList where
  canCastNode kd = kd == ExportListNd
  castNode = castKd ExportListNd ExportList
  nodeAst ast = ast.nodeA

instance AstNode ExportItem where
  canCastNode kd = kd == ExportItemNd
  castNode = castKd ExportItemNd ExportItem
  nodeAst ast = ast.nodeA

instance AstNode ImportDecl where
  canCastNode kd = kd == ImportDeclNd
  castNode = castKd ImportDeclNd ImportDecl
  nodeAst ast = ast.nodeA

instance AstNode ImportAlias where
  canCastNode kd = kd == ImportAliasNd
  castNode = castKd ImportAliasNd ImportAlias
  nodeAst ast = ast.nodeA

instance AstNode ImportItems where
  canCastNode kd = kd == ImportItemsNd
  castNode = castKd ImportItemsNd ImportItems
  nodeAst ast = ast.nodeA

instance AstNode ImportItem where
  canCastNode kd = kd == ImportItemNd
  castNode = castKd ImportItemNd ImportItem
  nodeAst ast = ast.nodeA

instance AstNode DocBlock where
  canCastNode kd = kd == DocBlockNd
  castNode = castKd DocBlockNd DocBlock
  nodeAst ast = ast.nodeA

instance AstNode Attr where
  canCastNode kd = kd == AttrNd
  castNode = castKd AttrNd Attr
  nodeAst ast = ast.nodeA

instance AstNode AttrList where
  canCastNode kd = kd == AttrListNd
  castNode = castKd AttrListNd AttrList
  nodeAst ast = ast.nodeA

instance AstNode Pragma where
  canCastNode kd = kd == PragmaNd
  castNode = castKd PragmaNd Pragma
  nodeAst ast = ast.nodeA

instance AstNode ValueDecl where
  canCastNode kd = kd == ValueDeclNd
  castNode = castKd ValueDeclNd ValueDecl
  nodeAst ast = ast.nodeA

instance AstNode TypeDecl where
  canCastNode kd = kd == TypeDeclNd
  castNode = castKd TypeDeclNd TypeDecl
  nodeAst ast = ast.nodeA

instance AstNode AliasDecl where
  canCastNode kd = kd == AliasDeclNd
  castNode = castKd AliasDeclNd AliasDecl
  nodeAst ast = ast.nodeA

instance AstNode EffectDecl where
  canCastNode kd = kd == EffectDeclNd
  castNode = castKd EffectDeclNd EffectDecl
  nodeAst ast = ast.nodeA

instance AstNode ForeignDecl where
  canCastNode kd = kd == ForeignDeclNd
  castNode = castKd ForeignDeclNd ForeignDecl
  nodeAst ast = ast.nodeA

instance AstNode FixityDecl where
  canCastNode kd = kd == FixityDeclNd
  castNode = castKd FixityDeclNd FixityDecl
  nodeAst ast = ast.nodeA

instance AstNode TypeAnn where
  canCastNode kd = kd == TypeAnnNd
  castNode = castKd TypeAnnNd TypeAnn
  nodeAst ast = ast.nodeA

instance AstNode TypeVar where
  canCastNode kd = kd == TypeVarNd
  castNode = castKd TypeVarNd TypeVar
  nodeAst ast = ast.nodeA

instance AstNode TypeCtor where
  canCastNode kd = kd == TypeCtorNd
  castNode = castKd TypeCtorNd TypeCtor
  nodeAst ast = ast.nodeA

instance AstNode TypeApply where
  canCastNode kd = kd == TypeApplyNd
  castNode = castKd TypeApplyNd TypeApply
  nodeAst ast = ast.nodeA

instance AstNode TypeTuple where
  canCastNode kd = kd == TypeTupleNd
  castNode = castKd TypeTupleNd TypeTuple
  nodeAst ast = ast.nodeA

instance AstNode TypeRecord where
  canCastNode kd = kd == TypeRecordNd
  castNode = castKd TypeRecordNd TypeRecord
  nodeAst ast = ast.nodeA

instance AstNode TypeField where
  canCastNode kd = kd == TypeFieldNd
  castNode = castKd TypeFieldNd TypeField
  nodeAst ast = ast.nodeA

instance AstNode TypeRow where
  canCastNode kd = kd == TypeRowNd
  castNode = castKd TypeRowNd TypeRow
  nodeAst ast = ast.nodeA

instance AstNode TypeArrow where
  canCastNode kd = kd == TypeArrowNd
  castNode = castKd TypeArrowNd TypeArrow
  nodeAst ast = ast.nodeA

instance AstNode TypeParen where
  canCastNode kd = kd == TypeParenNd
  castNode = castKd TypeParenNd TypeParen
  nodeAst ast = ast.nodeA

instance AstNode TypeEffect where
  canCastNode kd = kd == TypeEffectNd
  castNode = castKd TypeEffectNd TypeEffect
  nodeAst ast = ast.nodeA

instance AstNode WildcardPattern where
  canCastNode kd = kd == WildcardPatternNd
  castNode = castKd WildcardPatternNd WildcardPattern
  nodeAst ast = ast.nodeA

instance AstNode VarPattern where
  canCastNode kd = kd == VarPatternNd
  castNode = castKd VarPatternNd VarPattern
  nodeAst ast = ast.nodeA

instance AstNode CtorPattern where
  canCastNode kd = kd == CtorPatternNd
  castNode = castKd CtorPatternNd CtorPattern
  nodeAst ast = ast.nodeA

instance AstNode LiteralPattern where
  canCastNode kd = kd == LiteralPatternNd
  castNode = castKd LiteralPatternNd LiteralPattern
  nodeAst ast = ast.nodeA

instance AstNode TuplePattern where
  canCastNode kd = kd == TuplePatternNd
  castNode = castKd TuplePatternNd TuplePattern
  nodeAst ast = ast.nodeA

instance AstNode ListPattern where
  canCastNode kd = kd == ListPatternNd
  castNode = castKd ListPatternNd ListPattern
  nodeAst ast = ast.nodeA

instance AstNode RecordPattern where
  canCastNode kd = kd == RecordPatternNd
  castNode = castKd RecordPatternNd RecordPattern
  nodeAst ast = ast.nodeA

instance AstNode ConsPattern where
  canCastNode kd = kd == ConsPatternNd
  castNode = castKd ConsPatternNd ConsPattern
  nodeAst ast = ast.nodeA

instance AstNode AsPattern where
  canCastNode kd = kd == AsPatternNd
  castNode = castKd AsPatternNd AsPattern
  nodeAst ast = ast.nodeA

instance AstNode ParenPattern where
  canCastNode kd = kd == ParenPatternNd
  castNode = castKd ParenPatternNd ParenPattern
  nodeAst ast = ast.nodeA

instance AstNode NameExpr where
  canCastNode kd = kd == NameExprNd
  castNode = castKd NameExprNd NameExpr
  nodeAst ast = ast.nodeA

instance AstNode LiteralExpr where
  canCastNode kd = kd == LiteralExprNd
  castNode = castKd LiteralExprNd LiteralExpr
  nodeAst ast = ast.nodeA

instance AstNode HoleExpr where
  canCastNode kd = kd == HoleExprNd
  castNode = castKd HoleExprNd HoleExpr
  nodeAst ast = ast.nodeA

instance AstNode ParenExpr where
  canCastNode kd = kd == ParenExprNd
  castNode = castKd ParenExprNd ParenExpr
  nodeAst ast = ast.nodeA

instance AstNode TupleExpr where
  canCastNode kd = kd == TupleExprNd
  castNode = castKd TupleExprNd TupleExpr
  nodeAst ast = ast.nodeA

instance AstNode ListExpr where
  canCastNode kd = kd == ListExprNd
  castNode = castKd ListExprNd ListExpr
  nodeAst ast = ast.nodeA

instance AstNode RecordExpr where
  canCastNode kd = kd == RecordExprNd
  castNode = castKd RecordExprNd RecordExpr
  nodeAst ast = ast.nodeA

instance AstNode FieldRecord where
  canCastNode kd = kd == FieldRecordNd
  castNode = castKd FieldRecordNd FieldRecord
  nodeAst ast = ast.nodeA

instance AstNode UpdateRecordExpr where
  canCastNode kd = kd == UpdateRecordExprNd
  castNode = castKd UpdateRecordExprNd UpdateRecordExpr
  nodeAst ast = ast.nodeA

instance AstNode LambdaExpr where
  canCastNode kd = kd == LambdaExprNd
  castNode = castKd LambdaExprNd LambdaExpr
  nodeAst ast = ast.nodeA

instance AstNode ApplyExpr where
  canCastNode kd = kd == ApplyExprNd
  castNode = castKd ApplyExprNd ApplyExpr
  nodeAst ast = ast.nodeA

instance AstNode BinaryExpr where
  canCastNode kd = kd == BinaryExprNd
  castNode = castKd BinaryExprNd BinaryExpr
  nodeAst ast = ast.nodeA

instance AstNode IfExpr where
  canCastNode kd = kd == IfExprNd
  castNode = castKd IfExprNd IfExpr
  nodeAst ast = ast.nodeA

instance AstNode CaseExpr where
  canCastNode kd = kd == CaseExprNd
  castNode = castKd CaseExprNd CaseExpr
  nodeAst ast = ast.nodeA

instance AstNode BranchCase where
  canCastNode kd = kd == BranchCaseNd
  castNode = castKd BranchCaseNd BranchCase
  nodeAst ast = ast.nodeA

instance AstNode LetExpr where
  canCastNode kd = kd == LetExprNd
  castNode = castKd LetExprNd LetExpr
  nodeAst ast = ast.nodeA

instance AstNode BindingLet where
  canCastNode kd = kd == BindingLetNd
  castNode = castKd BindingLetNd BindingLet
  nodeAst ast = ast.nodeA

instance AstNode DoExpr where
  canCastNode kd = kd == DoExprNd
  castNode = castKd DoExprNd DoExpr
  nodeAst ast = ast.nodeA

instance AstNode StmtDo where
  canCastNode kd = kd == StmtDoNd
  castNode = castKd StmtDoNd StmtDo
  nodeAst ast = ast.nodeA

instance AstNode CatchDo where
  canCastNode kd = kd == ClauseCatchNd
  castNode = castKd ClauseCatchNd CatchDo
  nodeAst ast = ast.nodeA

instance AstNode AccessExpr where
  canCastNode kd = kd == AccessExprNd
  castNode = castKd AccessExprNd AccessExpr
  nodeAst ast = ast.nodeA

instance AstNode ExprMarkup where
  canCastNode kd = kd == ExprMarkupNd
  castNode = castKd ExprMarkupNd ExprMarkup
  nodeAst ast = ast.nodeA

instance AstNode ElemMarkup where
  canCastNode kd = kd == ElemMarkupNd
  castNode = castKd ElemMarkupNd ElemMarkup
  nodeAst ast = ast.nodeA

instance AstNode AttrMarkup where
  canCastNode kd = kd == AttrMarkupNd
  castNode = castKd AttrMarkupNd AttrMarkup
  nodeAst ast = ast.nodeA

instance AstNode TextMarkup where
  canCastNode kd = kd == TextMarkupNd
  castNode = castKd TextMarkupNd TextMarkup
  nodeAst ast = ast.nodeA

instance AstNode InterpMarkup where
  canCastNode kd = kd == InterpMarkupNd
  castNode = castKd InterpMarkupNd InterpMarkup
  nodeAst ast = ast.nodeA

instance AstNode FragmentMarkup where
  canCastNode kd = kd == FragmentMarkupNd
  castNode = castKd FragmentMarkupNd FragmentMarkup
  nodeAst ast = ast.nodeA

instance AstNode BlockForeign where
  canCastNode kd = kd == BlockForeignNd
  castNode = castKd BlockForeignNd BlockForeign
  nodeAst ast = ast.nodeA

instance AstNode ExprForeign where
  canCastNode kd = kd == ExprForeignNd
  castNode = castKd ExprForeignNd ExprForeign
  nodeAst ast = ast.nodeA

instance AstNode MissingNode where
  canCastNode kd = kd == MissingNd
  castNode = castKd MissingNd MissingNode
  nodeAst ast = ast.nodeA

instance AstNode ErrorNode where
  canCastNode kd = kd == ErrorNd
  castNode = castKd ErrorNd ErrorNode
  nodeAst ast = ast.nodeA

instance AstNode SkippedNode where
  canCastNode kd = kd == SkippedNd
  castNode = castKd SkippedNd SkippedNode
  nodeAst ast = ast.nodeA