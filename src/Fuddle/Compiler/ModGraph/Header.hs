{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

module Fuddle.Compiler.ModGraph.Header
  ( HeaderMod(..)
  , ImportHdrMod(..)
  , AliasHdrMod(..)
  , ExposeHdrMod(..)
  , ErrHeaderMod(..)
  , scanHeaderMod
  ) where

import Control.Applicative ((<|>), many, optional, some)
import Control.Monad (void)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word64)
import Data.List.NonEmpty (NonEmpty(..))

import Fuddle.Compiler.Base.Core (Hash64(..), fromIntSize)
import Fuddle.Compiler.Base.Range (Range, mkRange)
import Fuddle.Compiler.ModGraph.Name (ModName(..), QualImportModName(..), SegModName(..), textModName)
import Fuddle.Compiler.ModGraph.Origin (OriginMod(..), PkgRefMod(..), RootKindMod(..), SourceLocMod(..))
import Fuddle.Compiler.ModGraph.Types (EdgeId(..))
import Text.Megaparsec
  ( Parsec
  , chunk
  , eof
  , errorBundlePretty
  , getOffset
  , lookAhead
  , match
  , notFollowedBy
  , parse
  , satisfy
  , sepBy
  , try
  , (<?>), ParseErrorBundle
  )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

data AliasHdrMod = AliasHdrMod
  { name :: !Text
  }
  deriving stock (Eq, Show)

data ExposeHdrMod
  = OpenExposeHdrMod
  | ItemsExposeHdrMod !(Vector Text)
  deriving stock (Eq, Show)

data ImportHdrMod = ImportHdrMod
  { uid :: !EdgeId
  , target :: !QualImportModName
  , aliasMay :: !(Maybe AliasHdrMod)
  , exposeMay :: !(Maybe ExposeHdrMod)
  , rangeMay :: !(Maybe Range)
  }
  deriving stock (Eq, Show)

data HeaderMod = HeaderMod
  { name :: !ModName
  , imports :: !(Vector ImportHdrMod)
  , origin :: !OriginMod
  , hashHeader :: !Hash64
  }
  deriving stock (Eq, Show)

data ErrHeaderMod
  = InvalidModuleHeadErrHeaderMod !Text
  | MissingModuleHeadErrHeaderMod
  | InvalidImportHeadErrHeaderMod !Text
  deriving stock (Eq, Show)

scanHeaderMod :: OriginMod -> Text -> Either ErrHeaderMod HeaderMod
scanHeaderMod origin0 src0 =
  case parse (match (scnHdrP *> moduleDeclHdrP)) (sourceNameHdr origin0) src0 of
    Left errBundle
      | startsModuleHdr src0 ->
          Left (InvalidModuleHeadErrHeaderMod (bundleTextHdr errBundle))
      | otherwise ->
          Left MissingModuleHeadErrHeaderMod
    Right (prefixTxt, name0) ->
      let baseOff = T.length prefixTxt
          restTxt = T.drop baseOff src0
      in
      case parse (importsHdrP origin0 baseOff) (sourceNameHdr origin0) restTxt of
        Left errBundle ->
          Left (InvalidImportHeadErrHeaderMod (bundleTextHdr errBundle))
        Right imports0 ->
          let imports1 = V.fromList imports0
          in
          Right HeaderMod
            { name = name0
            , imports = imports1
            , origin = origin0
            , hashHeader = hashHeaderHdr name0 imports0
            }

type ParserHdr = Parsec Void Text

moduleDeclHdrP :: ParserHdr ModName
moduleDeclHdrP = do
  keywordHdrP "module"
  scnHdrP
  name0 <- modNameHdrP
  scnHdrP
  keywordHdrP "exposing"
  scnHdrP
  _ <- exposeListHdrP
  pure name0

importsHdrP :: OriginMod -> Int -> ParserHdr [ImportHdrMod]
importsHdrP origin0 baseOff = go 0 []
  where
    go !ix acc = do
      scnHdrP
      done <- endHdrP
      if done
        then pure (reverse acc)
        else do
          hasImport <- nextImportHdrP
          if hasImport
            then do
              imp0 <- importDeclHdrP origin0 baseOff ix
              go (ix + 1) (imp0 : acc)
            else pure (reverse acc)

importDeclHdrP :: OriginMod -> Int -> Int -> ParserHdr ImportHdrMod
importDeclHdrP origin0 baseOff impIx = do
  startOff0 <- getOffset
  (matchedTxt, (target0, aliasMay0, exposeMay0)) <- match importDeclBodyHdrP
  scnHdrP
  let endOff0 = startOff0 + T.length matchedTxt
  pure ImportHdrMod
    { uid = edgeIdHdr origin0 impIx
    , target = target0
    , aliasMay = aliasMay0
    , exposeMay = exposeMay0
    , rangeMay = Just (rangeAbsHdr baseOff startOff0 endOff0)
    }

importDeclBodyHdrP :: ParserHdr (QualImportModName, Maybe AliasHdrMod, Maybe ExposeHdrMod)
importDeclBodyHdrP = do
  keywordHdrP "import"
  scnHdrP
  target0 <- qualImportHdrP
  aliasMay0 <- optional (try aliasHdrP)
  exposeMay0 <- optional (try exposeHdrP)
  pure (target0, aliasMay0, exposeMay0)

aliasHdrP :: ParserHdr AliasHdrMod
aliasHdrP = do
  scnHdrP
  keywordHdrP "as"
  scnHdrP
  name0 <- upperIdentHdrP
  pure AliasHdrMod { name = name0 }

exposeHdrP :: ParserHdr ExposeHdrMod
exposeHdrP = do
  scnHdrP
  keywordHdrP "exposing"
  scnHdrP
  exposeListHdrP

exposeListHdrP :: ParserHdr ExposeHdrMod
exposeListHdrP = do
  _ <- char '('
  scnHdrP
  expose0 <- openExposeHdrP <|> itemsExposeHdrP
  scnHdrP
  _ <- char ')'
  pure expose0

openExposeHdrP :: ParserHdr ExposeHdrMod
openExposeHdrP = OpenExposeHdrMod <$ chunk ".."

itemsExposeHdrP :: ParserHdr ExposeHdrMod
itemsExposeHdrP = do
  items0 <- sepBy exposeItemHdrP (scnHdrP *> char ',' *> scnHdrP)
  pure (ItemsExposeHdrMod (V.fromList items0))

exposeItemHdrP :: ParserHdr Text
exposeItemHdrP = try upperExposeItemHdrP <|> lowerIdentHdrP

upperExposeItemHdrP :: ParserHdr Text
upperExposeItemHdrP = do
  name0 <- upperIdentHdrP
  _ <- optional (try ctorExposeSuffixHdrP)
  pure name0

ctorExposeSuffixHdrP :: ParserHdr ()
ctorExposeSuffixHdrP = do
  scnHdrP
  _ <- char '('
  scnHdrP
  _ <- chunk ".."
  scnHdrP
  _ <- char ')'
  pure ()

qualImportHdrP :: ParserHdr QualImportModName
qualImportHdrP = do
  pkgMay0 <- optional (try (packageQualHdrP <* char ':'))
  modName0 <- modNameHdrP
  pure QualImportModName
    { pkgMay = pkgMay0
    , modName = modName0
    }

modNameHdrP :: ParserHdr ModName
modNameHdrP = do
  headSeg0 <- upperIdentHdrP
  tailSegs0 <- many (char '.' *> upperIdentHdrP)
  pure (ModName (SegModName headSeg0 :| fmap SegModName tailSegs0))

upperIdentHdrP :: ParserHdr Text
upperIdentHdrP = identHdrP isUpperStartHdr isIdentTailHdrChar <?> "uppercase identifier"

lowerIdentHdrP :: ParserHdr Text
lowerIdentHdrP = identHdrP isLowerStartHdr isIdentTailHdrChar <?> "lowercase identifier"

packageQualHdrP :: ParserHdr Text
packageQualHdrP = identHdrP isPkgStartHdr isPkgTailHdrChar <?> "package qualifier"

identHdrP :: (Char -> Bool) -> (Char -> Bool) -> ParserHdr Text
identHdrP isHead0 isTail0 = do
  headCh0 <- satisfy isHead0
  tailTxt0 <- T.pack <$> many (satisfy isTail0)
  pure (T.cons headCh0 tailTxt0)

keywordHdrP :: Text -> ParserHdr ()
keywordHdrP kw0 = do
  _ <- chunk kw0
  notFollowedBy (satisfy isIdentTailHdrChar)

nextImportHdrP :: ParserHdr Bool
nextImportHdrP = do
  res0 <- optional (lookAhead (keywordHdrP "import"))
  pure (maybe False (const True) res0)

startsModuleHdr :: Text -> Bool
startsModuleHdr src0 =
  case parse parser0 "<header-prefix>" src0 of
    Right res0 -> res0
    Left _ -> False
  where
    parser0 = do
      scnHdrP
      res0 <- optional (keywordHdrP "module")
      pure (maybe False (const True) res0)

endHdrP :: ParserHdr Bool
endHdrP = do
  res0 <- optional eof
  pure (maybe False (const True) res0)

scnHdrP :: ParserHdr ()
scnHdrP = L.space spaceHdrP (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

spaceHdrP :: ParserHdr ()
spaceHdrP = void (some (satisfy isSpaceHdrChar))

isSpaceHdrChar :: Char -> Bool
isSpaceHdrChar ch0 = ch0 == '\xfeff' || ch0 == ' ' || ch0 == '\t' || ch0 == '\r' || ch0 == '\n'

isUpperStartHdr :: Char -> Bool
isUpperStartHdr ch0 = ch0 >= 'A' && ch0 <= 'Z'

isLowerStartHdr :: Char -> Bool
isLowerStartHdr ch0 = ch0 == '_' || (ch0 >= 'a' && ch0 <= 'z')

isIdentTailHdrChar :: Char -> Bool
isIdentTailHdrChar ch0 =
  (ch0 >= 'a' && ch0 <= 'z')
  || (ch0 >= 'A' && ch0 <= 'Z')
  || (ch0 >= '0' && ch0 <= '9')
  || ch0 == '_'
  || ch0 == '\''

isPkgStartHdr :: Char -> Bool
isPkgStartHdr ch0 = isLowerStartHdr ch0

isPkgTailHdrChar :: Char -> Bool
isPkgTailHdrChar ch0 = isIdentTailHdrChar ch0 || ch0 == '-' || ch0 == '.'

rangeAbsHdr :: Int -> Int -> Int -> Range
rangeAbsHdr baseOff startOff endOff =
  mkRange (fromIntSize (baseOff + startOff)) (fromIntSize (baseOff + endOff))

edgeIdHdr :: OriginMod -> Int -> EdgeId
edgeIdHdr origin0 impIx =
  let seedTxt = renderOriginSeedHdr origin0 <> "#imp:" <> showTextHdr impIx
  in EdgeId (hashWord64Hdr seedTxt)

hashHeaderHdr :: ModName -> [ImportHdrMod] -> Hash64
hashHeaderHdr name0 imports0 = Hash64 (hashWord64Hdr (serializeHeaderHdr name0 imports0))

serializeHeaderHdr :: ModName -> [ImportHdrMod] -> Text
serializeHeaderHdr name0 imports0 =
  T.concat ("mod{" : textModName name0 : "}" : fmap serializeImportHdr imports0)

serializeImportHdr :: ImportHdrMod -> Text
serializeImportHdr import0 =
  T.concat
    [ "|imp{"
    , renderQualImportHdr import0.target
    , "}{"
    , maybe "" (.name) import0.aliasMay
    , "}{"
    , renderExposeHdr import0.exposeMay
    , "}"
    ]

renderQualImportHdr :: QualImportModName -> Text
renderQualImportHdr qual0 =
  case qual0.pkgMay of
    Nothing -> textModName qual0.modName
    Just pkg0 -> pkg0 <> ":" <> textModName qual0.modName

renderExposeHdr :: Maybe ExposeHdrMod -> Text
renderExposeHdr exposeMay0 =
  case exposeMay0 of
    Nothing -> ""
    Just OpenExposeHdrMod -> ".."
    Just (ItemsExposeHdrMod items0) -> T.intercalate "," (V.toList items0)

hashWord64Hdr :: Text -> Word64
hashWord64Hdr txt0 = BS.foldl' stepHdr fnvOffsetHdr (TE.encodeUtf8 txt0)
  where
    stepHdr !acc0 !byte0 = (acc0 `xor` fromIntegral byte0) * fnvPrimeHdr

fnvOffsetHdr :: Word64
fnvOffsetHdr = 14695981039346656037

fnvPrimeHdr :: Word64
fnvPrimeHdr = 1099511628211

bundleTextHdr :: ParseErrorBundle Text Void -> Text
bundleTextHdr errBundle = T.stripEnd (T.pack (errorBundlePretty errBundle))

sourceNameHdr :: OriginMod -> String
sourceNameHdr origin0 = T.unpack (renderOriginSeedHdr origin0)

renderOriginSeedHdr :: OriginMod -> Text
renderOriginSeedHdr origin0 =
  T.intercalate "|"
    [ "root=" <> showTextHdr origin0.root
    , "rootKind=" <> renderRootKindHdr origin0.rootKind
    , "pkg=" <> renderPkgRefHdr origin0.pkg
    , "loc=" <> renderSourceLocHdr origin0.loc
    ]

renderRootKindHdr :: RootKindMod -> Text
renderRootKindHdr rootKind0 =
  case rootKind0 of
    WorkspaceRootMod -> "workspace"
    PackageRootMod -> "package"
    RegistryRootMod -> "registry"
    VirtualRootMod -> "virtual"

renderPkgRefHdr :: PkgRefMod -> Text
renderPkgRefHdr pkg0 =
  T.intercalate "@"
    [ pkg0.name
    , pkg0.version
    , showTextHdr pkg0.uid
    ]

renderSourceLocHdr :: SourceLocMod -> Text
renderSourceLocHdr sourceLoc0 =
  case sourceLoc0 of
    FileSourceLocMod filePath0 ->
      T.pack filePath0
    ArchiveSourceLocMod archive0 filePath0 ->
      archive0 <> ":" <> T.pack filePath0
    VirtualSourceLocMod name0 ->
      "<" <> name0 <> ">"

showTextHdr :: Show a => a -> Text
showTextHdr = T.pack . show