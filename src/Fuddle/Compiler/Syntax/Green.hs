{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Syntax.Green
  ( GreenNodeId(..)
  , GreenTokId(..)
  , ChildIx(..)
  , NodeFlags(..)
  , recoveryNF
  , layoutNF
  , hasNodeFlag
  , GreenTok(..)
  , GreenNode(..)
  , GreenElem(..)
  , GreenArena(..)
  , GreenFile(..)
  , emptyArena
  , nodeCount
  , tokCount
  , childCount
  , lookupNode
  , lookupTokGreen
  , lookupChild
  , childrenNode
  , elemWidth
  ) where

import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16, Word32)
import Fuddle.Compiler.Base.Core (Hash64, TextSize)
import Fuddle.Compiler.Syntax.Kind (SyntaxKind)
import Fuddle.Compiler.Syntax.Token (LexemeRef, TokenFlags, TokenOrigin)

newtype GreenNodeId = GreenNodeId Int32
  deriving stock (Eq, Ord, Show)

newtype GreenTokId = GreenTokId Int32
  deriving stock (Eq, Ord, Show)

newtype ChildIx = ChildIx Int32
  deriving stock (Eq, Ord, Show)

newtype NodeFlags = NodeFlags Word16
  deriving stock (Eq, Ord, Show)

recoveryNF :: NodeFlags
recoveryNF = NodeFlags 0x0001

layoutNF :: NodeFlags
layoutNF = NodeFlags 0x0002

hasNodeFlag :: NodeFlags -> NodeFlags -> Bool
hasNodeFlag (NodeFlags xs) (NodeFlags ys) = (xs .&. ys) == ys

data GreenTok = GreenTok
  { kindGT :: !SyntaxKind
  , widthGT :: !TextSize
  , lexemeRefGT :: !LexemeRef
  , originGT :: !TokenOrigin
  , flagsGT :: !TokenFlags
  }
  deriving stock (Eq, Show)

data GreenNode = GreenNode
  { kindGN :: !SyntaxKind
  , widthGN :: !TextSize
  , firstChildGN :: !ChildIx
  , childCountGN :: !Word32
  , hashGN :: !Hash64
  , flagsGN :: !NodeFlags
  }
  deriving stock (Eq, Show)

data GreenElem
  = NodeGE !GreenNodeId
  | TokGE !GreenTokId
  deriving stock (Eq, Show)

data GreenArena = GreenArena
  { nodesGA :: !(Vector GreenNode)
  , toksGA :: !(Vector GreenTok)
  , childrenGA :: !(Vector GreenElem)
  }
  deriving stock (Eq, Show)

data GreenFile = GreenFile
  { arenaGF :: !GreenArena
  , rootGF :: !GreenNodeId
  , hashSourceGF :: !Hash64
  }
  deriving stock (Eq, Show)

emptyArena :: GreenArena
emptyArena = GreenArena { nodesGA = V.empty, toksGA = V.empty, childrenGA = V.empty }

nodeCount :: GreenArena -> Int
nodeCount arena = V.length arena.nodesGA

tokCount :: GreenArena -> Int
tokCount arena = V.length arena.toksGA

childCount :: GreenArena -> Int
childCount arena = V.length arena.childrenGA

lookupNode :: GreenArena -> GreenNodeId -> GreenNode
lookupNode arena nodeId =
  let ix = idxNode nodeId
  in case arena.nodesGA V.!? ix of
       Just node -> node
       Nothing -> errGreen $
         "lookupNode: node index out of range: " <> show nodeId
           <> ", node count = " <> show (nodeCount arena)

lookupTokGreen :: GreenArena -> GreenTokId -> GreenTok
lookupTokGreen arena tokId =
  let ix = idxTok tokId
  in case arena.toksGA V.!? ix of
       Just tok -> tok
       Nothing -> errGreen $
         "lookupTokGreen: token index out of range: " <> show tokId
           <> ", token count = " <> show (tokCount arena)

lookupChild :: GreenArena -> ChildIx -> GreenElem
lookupChild arena childIx =
  let ix = idxChild childIx
  in case arena.childrenGA V.!? ix of
       Just elem0 -> elem0
       Nothing -> errGreen $
         "lookupChild: child index out of range: " <> show childIx
           <> ", child count = " <> show (childCount arena)

childrenNode :: GreenArena -> GreenNode -> Vector GreenElem
childrenNode arena node =
  let firstIx = idxChild node.firstChildGN
      childLen = countWord32 "childrenNode" node.childCountGN
      total = childCount arena
      endIx = toInteger firstIx + toInteger childLen
  in if firstIx <= total && endIx <= toInteger total
       then V.slice firstIx childLen arena.childrenGA
       else errGreen $
         "childrenNode: child slice out of range for node kind "
           <> show node.kindGN
           <> ", firstChild = " <> show node.firstChildGN
           <> ", childCount = " <> show node.childCountGN
           <> ", arena child count = " <> show total

elemWidth :: GreenArena -> GreenElem -> TextSize
elemWidth arena elem0 =
  case elem0 of
    NodeGE nodeId -> (lookupNode arena nodeId).widthGN
    TokGE tokId -> (lookupTokGreen arena tokId).widthGT

idxNode :: GreenNodeId -> Int
idxNode (GreenNodeId ix)
  | ix < 0 = errGreen $ "idxNode: negative node index: " <> show ix
  | otherwise = fromIntegral ix

idxTok :: GreenTokId -> Int
idxTok (GreenTokId ix)
  | ix < 0 = errGreen $ "idxTok: negative token index: " <> show ix
  | otherwise = fromIntegral ix

idxChild :: ChildIx -> Int
idxChild (ChildIx ix)
  | ix < 0 = errGreen $ "idxChild: negative child index: " <> show ix
  | otherwise = fromIntegral ix

countWord32 :: String -> Word32 -> Int
countWord32 fun n
  | toInteger n > toInteger (maxBound :: Int) =
      errGreen $ fun <> ": count exceeds Int range: " <> show n
  | otherwise = fromIntegral n

errGreen :: String -> a
errGreen msg = error ("Fuddle.Compiler.Syntax.Green." <> msg)