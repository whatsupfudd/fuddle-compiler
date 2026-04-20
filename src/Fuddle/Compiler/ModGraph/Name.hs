{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.ModGraph.Name
  ( SegModName(..)
  , ModName(..)
  , QualImportModName(..)
  , mkModName
  , textModName
  , segsModName
  , parentModNameMay
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

newtype SegModName = SegModName Text
  deriving stock (Eq, Ord, Show)

newtype ModName = ModName (NonEmpty SegModName)
  deriving stock (Eq, Ord, Show)

data QualImportModName = QualImportModName
  { pkgMay :: !(Maybe Text)
  , modName :: !ModName
  }
  deriving stock (Eq, Ord, Show)

mkModName :: SegModName -> [SegModName] -> ModName
mkModName seg0 segs0 = ModName (seg0 :| segs0)

textModName :: ModName -> Text
textModName (ModName segs0) =
  T.intercalate "." (fmap textSegModName (NE.toList segs0))

segsModName :: ModName -> NonEmpty SegModName
segsModName (ModName segs0) = segs0

parentModNameMay :: ModName -> Maybe ModName
parentModNameMay (ModName segs0) = ModName <$> initNonEmptyMay segs0

textSegModName :: SegModName -> Text
textSegModName (SegModName txt) = txt

initNonEmptyMay :: NonEmpty a -> Maybe (NonEmpty a)
initNonEmptyMay (item0 :| rest0) =
  case rest0 of
    [] -> Nothing
    item1 : rest1 -> Just (item0 :| go item1 rest1)
  where
    go :: a -> [a] -> [a]
    go itemN restN =
      case restN of
        [] -> []
        itemNext : restNext -> itemN : go itemNext restNext