{-# LANGUAGE DerivingStrategies #-}
module Fuddle.Compiler.Syntax.Red
  ( TreeSyntax
  , ElemSyntax(..)
  , NodeSyntax
  , TokenSyntax
  , mkTree
  , rootNode
  , kindNode
  , kindToken
  , rangeNode
  , rangeToken
  , textNode
  , textToken
  , parentNodeMay
  , childrenNode
  , childNodes
  , childTokens
  , firstTokenMay
  , lastTokenMay
  , nextTokenMay
  , prevTokenMay
  , tokenAtMay
  , coverNode
  , ancestorsNode
  ) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, gets, modify', runState, state)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fuddle.Compiler.Base.Core (TextSize, maxSize, minSize, plusSize, zeroSize)
import Fuddle.Compiler.Base.Range (Range(..), containsOffset, containsRange, mkRange, widthRange)
import Fuddle.Compiler.Source.Buffer (SnapshotSrc(..), sliceTextBuf)
import qualified Fuddle.Compiler.Syntax.Green as G
import Fuddle.Compiler.Syntax.Kind (SyntaxKind)

data TreeSyntax = TreeSyntax
  { snapTS :: !SnapshotSrc
  , greenTS :: !G.GreenFile
  , nodesTS :: !(Vector NodeRec)
  , toksTS :: !(Vector TokRec)
  , rootTS :: !NodeIxR
  }
  deriving stock (Eq, Show)

data NodeSyntax = NodeSyntax
  { treeNS :: !TreeSyntax
  , ixNS :: !NodeIxR
  }

data TokenSyntax = TokenSyntax
  { treeTK :: !TreeSyntax
  , ixTK :: !TokIxR
  }

data ElemSyntax
  = NodeSE !NodeSyntax
  | TokSE !TokenSyntax
  deriving stock (Eq, Show)

instance Eq NodeSyntax where
  lhs == rhs = lhs.ixNS == rhs.ixNS && sameTree lhs.treeNS rhs.treeNS

instance Show NodeSyntax where
  show node =
    "NodeSyntax(ix=" <> showIxNodeR node.ixNS
      <> ", kind=" <> show (kindNode node)
      <> ", range=" <> show (rangeNode node)
      <> ")"

instance Eq TokenSyntax where
  lhs == rhs = lhs.ixTK == rhs.ixTK && sameTree lhs.treeTK rhs.treeTK

instance Show TokenSyntax where
  show tok =
    "TokenSyntax(ix=" <> showIxTokR tok.ixTK
      <> ", kind=" <> show (kindToken tok)
      <> ", range=" <> show (rangeToken tok)
      <> ")"

newtype NodeIxR = NodeIxR Int
  deriving stock (Eq, Ord, Show)

newtype TokIxR = TokIxR Int
  deriving stock (Eq, Ord, Show)

data ElemRec
  = NodeER !NodeIxR
  | TokER !TokIxR
  deriving stock (Eq, Show)

data NodeRec = NodeRec
  { kindNR :: !SyntaxKind
  , parentNR :: !(Maybe NodeIxR)
  , rangeNR :: !Range
  , elemsNR :: !(Vector ElemRec)
  , firstTokNR :: !(Maybe TokIxR)
  , lastTokNR :: !(Maybe TokIxR)
  , greenNR :: !G.GreenNodeId
  }
  deriving stock (Eq, Show)

data TokRec = TokRec
  { kindTR :: !SyntaxKind
  , parentTR :: !NodeIxR
  , rangeTR :: !Range
  , prevTR :: !(Maybe TokIxR)
  , nextTR :: !(Maybe TokIxR)
  , greenTR :: !G.GreenTokId
  }
  deriving stock (Eq, Show)

data BuildSt = BuildSt
  { nextNodeBS :: !Int
  , nextTokBS :: !Int
  , nodesBS :: !(IntMap NodeRec)
  , toksBS :: !(IntMap TokRec)
  , lastTokBS :: !(Maybe TokIxR)
  }

mkTree :: SnapshotSrc -> G.GreenFile -> TreeSyntax
mkTree snap green =
  let (rootIx, st) = runState (buildNode green.arenaGF Nothing zeroSize green.rootGF) emptyBuildSt
  in TreeSyntax
      { snapTS = snap
      , greenTS = green
      , nodesTS = V.fromList (IM.elems st.nodesBS)
      , toksTS = V.fromList (IM.elems st.toksBS)
      , rootTS = rootIx
      }

rootNode :: TreeSyntax -> NodeSyntax
rootNode tree = wrapNode tree tree.rootTS

kindNode :: NodeSyntax -> SyntaxKind
kindNode node = (nodeRec node).kindNR

kindToken :: TokenSyntax -> SyntaxKind
kindToken tok = (tokRec tok).kindTR

rangeNode :: NodeSyntax -> Range
rangeNode node = (nodeRec node).rangeNR

rangeToken :: TokenSyntax -> Range
rangeToken tok = (tokRec tok).rangeTR

textNode :: NodeSyntax -> Text
textNode node = sliceTextBuf node.treeNS.snapTS.bufSS (rangeNode node)

textToken :: TokenSyntax -> Text
textToken tok = sliceTextBuf tok.treeTK.snapTS.bufSS (rangeToken tok)

parentNodeMay :: NodeSyntax -> Maybe NodeSyntax
parentNodeMay node = fmap (wrapNode node.treeNS) (nodeRec node).parentNR

childrenNode :: NodeSyntax -> Vector ElemSyntax
childrenNode node = V.map (wrapElem node.treeNS) (nodeRec node).elemsNR

childNodes :: NodeSyntax -> Vector NodeSyntax
childNodes node =
  V.mapMaybe
    (\elem0 ->
      case elem0 of
        NodeER nodeIx -> Just (wrapNode node.treeNS nodeIx)
        TokER _ -> Nothing
    )
    (nodeRec node).elemsNR

childTokens :: NodeSyntax -> Vector TokenSyntax
childTokens node =
  V.mapMaybe
    (\elem0 ->
      case elem0 of
        NodeER _ -> Nothing
        TokER tokIx -> Just (wrapTok node.treeNS tokIx)
    )
    (nodeRec node).elemsNR

firstTokenMay :: NodeSyntax -> Maybe TokenSyntax
firstTokenMay node = fmap (wrapTok node.treeNS) (nodeRec node).firstTokNR

lastTokenMay :: NodeSyntax -> Maybe TokenSyntax
lastTokenMay node = fmap (wrapTok node.treeNS) (nodeRec node).lastTokNR

nextTokenMay :: TokenSyntax -> Maybe TokenSyntax
nextTokenMay tok = fmap (wrapTok tok.treeTK) (tokRec tok).nextTR

prevTokenMay :: TokenSyntax -> Maybe TokenSyntax
prevTokenMay tok = fmap (wrapTok tok.treeTK) (tokRec tok).prevTR

tokenAtMay :: TreeSyntax -> TextSize -> Maybe TokenSyntax
tokenAtMay tree off0 =
  let off = clampOffsetTS tree off0
  in case findTokContaining tree off of
      Just tokIx -> Just (wrapTok tree tokIx)
      Nothing ->
        case findTokEndingAt tree off of
          Just tokIx -> Just (wrapTok tree tokIx)
          Nothing ->
            case findTokStartingAt tree off of
              Just tokIx -> Just (wrapTok tree tokIx)
              Nothing -> fmap (wrapTok tree) (findTokZeroWidthAt tree off)

coverNode :: TreeSyntax -> Range -> NodeSyntax
coverNode tree range0 = goCover (rootNode tree) (clampRangeTS tree range0)
  where
    goCover node target =
      case coverChildMay node target of
        Nothing -> node
        Just child -> goCover child target

ancestorsNode :: NodeSyntax -> Vector NodeSyntax
ancestorsNode node = V.fromList (go node)
  where
    go cur =
      case parentNodeMay cur of
        Nothing -> [cur]
        Just parent -> cur : go parent

emptyBuildSt :: BuildSt
emptyBuildSt =
  BuildSt
    { nextNodeBS = 0
    , nextTokBS = 0
    , nodesBS = IM.empty
    , toksBS = IM.empty
    , lastTokBS = Nothing
    }

buildNode :: G.GreenArena -> Maybe NodeIxR -> TextSize -> G.GreenNodeId -> State BuildSt NodeIxR
buildNode arena parent off greenIx = do
  let greenNode = G.lookupNode arena greenIx
  nodeIx <- freshNodeIx
  let emptyNode =
        NodeRec
          { kindNR = greenNode.kindGN
          , parentNR = parent
          , rangeNR = mkRange off off
          , elemsNR = V.empty
          , firstTokNR = Nothing
          , lastTokNR = Nothing
          , greenNR = greenIx
          }
  putNodeRec nodeIx emptyNode
  let greenElems = childrenGreen arena greenNode
  (endOff, elems, firstTok, lastTok) <- walkElems arena nodeIx off (V.toList greenElems)
  let fullNode =
        NodeRec
          { kindNR = greenNode.kindGN
          , parentNR = parent
          , rangeNR = mkRange off endOff
          , elemsNR = V.fromList elems
          , firstTokNR = firstTok
          , lastTokNR = lastTok
          , greenNR = greenIx
          }
  putNodeRec nodeIx fullNode
  pure nodeIx

walkElems :: G.GreenArena -> NodeIxR -> TextSize -> [G.GreenElem] -> State BuildSt (TextSize, [ElemRec], Maybe TokIxR, Maybe TokIxR)
walkElems _ _ off [] = pure (off, [], Nothing, Nothing)
walkElems arena parent off (elem0 : rest) =
  case elem0 of
    G.NodeGE greenIx -> do
      childIx <- buildNode arena (Just parent) off greenIx
      childRec0 <- gets (lookupNodeRec childIx . nodesBS)
      (endOff, elemsRest, firstRest, lastRest) <- walkElems arena parent childRec0.rangeNR.end rest
      let firstTok = childRec0.firstTokNR <|> firstRest
      let lastTok = lastRest <|> childRec0.lastTokNR
      pure (endOff, NodeER childIx : elemsRest, firstTok, lastTok)
    G.TokGE greenIx -> do
      tokIx <- buildTok arena parent off greenIx
      tokRec0 <- gets (lookupTokRec tokIx . toksBS)
      (endOff, elemsRest, firstRest, lastRest) <- walkElems arena parent tokRec0.rangeTR.end rest
      let firstTok = Just tokIx
      let lastTok = lastRest <|> Just tokIx
      pure (endOff, TokER tokIx : elemsRest, firstTok <|> firstRest, lastTok)

buildTok :: G.GreenArena -> NodeIxR -> TextSize -> G.GreenTokId -> State BuildSt TokIxR
buildTok arena parent off greenIx = do
  let greenTok = G.lookupTokGreen arena greenIx
  prevTok <- gets (.lastTokBS)
  tokIx <- freshTokIx
  let tokRec0 =
        TokRec
          { kindTR = greenTok.kindGT
          , parentTR = parent
          , rangeTR = mkRange off (plusSize off greenTok.widthGT)
          , prevTR = prevTok
          , nextTR = Nothing
          , greenTR = greenIx
          }
  modify' (\st -> st { toksBS = IM.insert (unTokIxR tokIx) tokRec0 st.toksBS, lastTokBS = Just tokIx })
  case prevTok of
    Nothing -> pure ()
    Just prevIx ->
      modify' (\st -> st { toksBS = IM.adjust (\tok -> tok { nextTR = Just tokIx }) (unTokIxR prevIx) st.toksBS })
  pure tokIx

freshNodeIx :: State BuildSt NodeIxR
freshNodeIx = state (\st -> (NodeIxR st.nextNodeBS, st { nextNodeBS = st.nextNodeBS + 1 }))

freshTokIx :: State BuildSt TokIxR
freshTokIx = state (\st -> (TokIxR st.nextTokBS, st { nextTokBS = st.nextTokBS + 1 }))

putNodeRec :: NodeIxR -> NodeRec -> State BuildSt ()
putNodeRec nodeIx nodeRec0 =
  modify' (\st -> st { nodesBS = IM.insert (unNodeIxR nodeIx) nodeRec0 st.nodesBS })

lookupNodeRec :: NodeIxR -> IntMap NodeRec -> NodeRec
lookupNodeRec nodeIx nodeMap =
  case IM.lookup (unNodeIxR nodeIx) nodeMap of
    Just nodeRec0 -> nodeRec0
    Nothing ->
      error ("Fuddle.Compiler.Syntax.Red.lookupNodeRec: missing node index " <> showIxNodeR nodeIx)

lookupTokRec :: TokIxR -> IntMap TokRec -> TokRec
lookupTokRec tokIx tokMap =
  case IM.lookup (unTokIxR tokIx) tokMap of
    Just tokRec0 -> tokRec0
    Nothing ->
      error ("Fuddle.Compiler.Syntax.Red.lookupTokRec: missing token index " <> showIxTokR tokIx)

childrenGreen :: G.GreenArena -> G.GreenNode -> Vector G.GreenElem
childrenGreen arena greenNode =
  let startIx =
        case greenNode.firstChildGN of
          G.ChildIx n -> fromIntegral n
      count = fromIntegral greenNode.childCountGN
  in V.slice startIx count arena.childrenGA

sameTree :: TreeSyntax -> TreeSyntax -> Bool
sameTree lhs rhs = lhs.snapTS == rhs.snapTS && lhs.greenTS == rhs.greenTS

wrapNode :: TreeSyntax -> NodeIxR -> NodeSyntax
wrapNode tree ix = NodeSyntax { treeNS = tree, ixNS = ix }

wrapTok :: TreeSyntax -> TokIxR -> TokenSyntax
wrapTok tree ix = TokenSyntax { treeTK = tree, ixTK = ix }

wrapElem :: TreeSyntax -> ElemRec -> ElemSyntax
wrapElem tree elem0 =
  case elem0 of
    NodeER ix -> NodeSE (wrapNode tree ix)
    TokER ix -> TokSE (wrapTok tree ix)

nodeRec :: NodeSyntax -> NodeRec
nodeRec node = nodeRecAt node.treeNS node.ixNS

tokRec :: TokenSyntax -> TokRec
tokRec tok = tokRecAt tok.treeTK tok.ixTK

nodeRecAt :: TreeSyntax -> NodeIxR -> NodeRec
nodeRecAt tree nodeIx = tree.nodesTS V.! unNodeIxR nodeIx

tokRecAt :: TreeSyntax -> TokIxR -> TokRec
tokRecAt tree tokIx = tree.toksTS V.! unTokIxR tokIx

findTokContaining :: TreeSyntax -> TextSize -> Maybe TokIxR
findTokContaining tree off =
  V.ifoldl'
    (\acc ix tokRec0 ->
      acc <|> if containsPointTok off tokRec0 then Just (TokIxR ix) else Nothing
    )
    Nothing
    tree.toksTS

findTokEndingAt :: TreeSyntax -> TextSize -> Maybe TokIxR
findTokEndingAt tree off =
  V.ifoldl'
    (\acc ix tokRec0 ->
      if isNonEmptyTok tokRec0 && tokRec0.rangeTR.end == off then Just (TokIxR ix) else acc
    )
    Nothing
    tree.toksTS

findTokStartingAt :: TreeSyntax -> TextSize -> Maybe TokIxR
findTokStartingAt tree off =
  V.ifoldl'
    (\acc ix tokRec0 ->
      acc <|> if isNonEmptyTok tokRec0 && tokRec0.rangeTR.start == off then Just (TokIxR ix) else Nothing
    )
    Nothing
    tree.toksTS

findTokZeroWidthAt :: TreeSyntax -> TextSize -> Maybe TokIxR
findTokZeroWidthAt tree off =
  V.ifoldl'
    (\acc ix tokRec0 ->
      acc <|> if isZeroWidthTok tokRec0 && tokRec0.rangeTR.start == off then Just (TokIxR ix) else Nothing
    )
    Nothing
    tree.toksTS

containsPointTok :: TextSize -> TokRec -> Bool
containsPointTok off tokRec0
  | isZeroWidthTok tokRec0 = tokRec0.rangeTR.start == off
  | otherwise = containsOffset tokRec0.rangeTR off

isZeroWidthTok :: TokRec -> Bool
isZeroWidthTok tokRec0 = widthRange tokRec0.rangeTR == zeroSize

isNonEmptyTok :: TokRec -> Bool
isNonEmptyTok tokRec0 = widthRange tokRec0.rangeTR > zeroSize

coverChildMay :: NodeSyntax -> Range -> Maybe NodeSyntax
coverChildMay node target =
  let covered =
        V.filter
          (\child -> containsRange (rangeNode child) target)
          (childNodes node)
  in if V.null covered
      then Nothing
      else Just (V.foldl1' narrowerNode covered)

narrowerNode :: NodeSyntax -> NodeSyntax -> NodeSyntax
narrowerNode lhs rhs =
  let lhsW = widthRange (rangeNode lhs)
      rhsW = widthRange (rangeNode rhs)
  in if rhsW < lhsW then rhs else lhs

clampRangeTS :: TreeSyntax -> Range -> Range
clampRangeTS tree range0 =
  mkRange
    (clampOffsetTS tree range0.start)
    (clampOffsetTS tree range0.end)

clampOffsetTS :: TreeSyntax -> TextSize -> TextSize
clampOffsetTS tree off = minSize tree.snapTS.sizeSS (maxSize zeroSize off)

unNodeIxR :: NodeIxR -> Int
unNodeIxR (NodeIxR ix) = ix

unTokIxR :: TokIxR -> Int
unTokIxR (TokIxR ix) = ix

showIxNodeR :: NodeIxR -> String
showIxNodeR (NodeIxR ix) = show ix

showIxTokR :: TokIxR -> String
showIxTokR (TokIxR ix) = show ix