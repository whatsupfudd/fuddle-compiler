{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Fuddle.Compiler.Syntax.Serialize
  ( VersionCst(..)
  , EncodeErr(..)
  , DecodeErr(..)
  , versionCurrent
  , encodeGreenFile
  , decodeGreenFile
  ) where

import Control.Monad (foldM, replicateM, unless, when)

import Data.Binary.Get (Get, getByteString, getWord8, getWord16le, getWord32le, getWord64le, runGetOrFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Coerce (coerce, Coercible)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)

import Fuddle.Compiler.Syntax.Green (
    ChildIx (..), GreenArena(..), GreenElem(..), GreenFile(..), GreenNode(..)
  , GreenNodeId (..), GreenTok(..), GreenTokId (..), NodeFlags (..)
  )
import Fuddle.Compiler.Syntax.Token (BlobKey (..), LexemeRef (..)
          , SyntheticReason (..), TokenFlags (..), TokenOrigin (..), TextKey (..))
import Fuddle.Compiler.Syntax.Kind (SyntaxKind, isNodeKd, isTokenKd, kindTag)
import Fuddle.Compiler.Base.Core (TextSize (..), Hash64 (..))

newtype VersionCst = VersionCst Word16
  deriving stock (Eq, Ord, Show)

data EncodeErr
  = CountRangeEE !Text !Int
  | NatRangeEE !Text !Integer
  | KindFamilyEE !Text !SyntaxKind
  | RootRangeEE !GreenNodeId !Int
  | ChildWindowEE !GreenNodeId !ChildIx !Word32 !Int
  | ChildElemEE !Int !GreenElem
  | WidthNodeEE !GreenNodeId !Word32 !Word32
  deriving stock (Eq, Show)

data DecodeErr
  = MagicMismatchDE !ByteString
  | VersionUnsupportedDE !VersionCst
  | GetFailDE !Text
  | TrailingBytesDE !Int
  | NatRangeDE !Text !Integer
  | TagInvalidDE !Text !Word16
  | KindTagDE !Text !Word16
  | KindFamilyDE !Text !SyntaxKind
  | RootRangeDE !GreenNodeId !Int
  | ChildWindowDE !GreenNodeId !ChildIx !Word32 !Int
  | ChildElemDE !Int !GreenElem
  | WidthNodeDE !GreenNodeId !Word32 !Word32
  deriving stock (Eq, Show)

data ErrOps e = ErrOps
  { natRangeEO :: Text -> Integer -> e
  , kindFamilyEO :: Text -> SyntaxKind -> e
  , rootRangeEO :: GreenNodeId -> Int -> e
  , childWindowEO :: GreenNodeId -> ChildIx -> Word32 -> Int -> e
  , childElemEO :: Int -> GreenElem -> e
  , widthNodeEO :: GreenNodeId -> Word32 -> Word32 -> e
  }

data HeaderRaw = HeaderRaw
  { nodesHR :: !Word32
  , toksHR :: !Word32
  , childrenHR :: !Word32
  , rootHR :: !Word32
  , sourceHashHR :: !Word64
  }

data BodyRaw = BodyRaw
  { nodesBR :: ![NodeRaw]
  , toksBR :: ![TokRaw]
  , childrenBR :: ![ElemRaw]
  }

data NodeRaw = NodeRaw
  { kindRN :: !Word16
  , widthRN :: !Word32
  , firstChildRN :: !Word32
  , childCountRN :: !Word32
  , hashRN :: !Word64
  , flagsRN :: !Word16
  }

data TokRaw = TokRaw
  { kindRT :: !Word16
  , widthRT :: !Word32
  , lexTagRT :: !Word8
  , lexPayloadRT :: !Word32
  , originTagRT :: !Word8
  , originPayloadRT :: !Word8
  , flagsRT :: !Word16
  }

data ElemRaw = ElemRaw
  { tagRE :: !Word8
  , payloadRE :: !Word32
  }


versionCurrent :: VersionCst
versionCurrent = VersionCst 1

encodeGreenFile :: GreenFile -> Either EncodeErr ByteString
encodeGreenFile green0 = do
  validateCountsEE green0
  validateFileGeneric opsEE green0
  pure (BL.toStrict (BB.toLazyByteString (buildFile green0)))

decodeGreenFile :: ByteString -> Either DecodeErr GreenFile
decodeGreenFile bytes0 = do
  let magicLen = BS.length magicCst
  let (magic0, rest0) = BS.splitAt magicLen bytes0
  unless (magic0 == magicCst) (Left (MagicMismatchDE magic0))

  (ver0, rest1) <- runStep getVersion rest0
  unless (ver0 == versionCurrent) (Left (VersionUnsupportedDE ver0))

  (head0, rest2) <- runStep getHeader rest1
  nodeCount0 <- countIntDE "node count" head0.nodesHR
  tokCount0 <- countIntDE "token count" head0.toksHR
  childCount0 <- countIntDE "child count" head0.childrenHR

  (body0, rest3) <- runStep (getBody nodeCount0 tokCount0 childCount0) rest2
  unless (BS.null rest3) (Left (TrailingBytesDE (BS.length rest3)))

  buildFileDE head0 body0

magicCst :: ByteString
magicCst = "FUDCST"

maxStoredI :: Integer
maxStoredI = toInteger (maxBound :: Int32)


opsEE :: ErrOps EncodeErr
opsEE =
  ErrOps
    { natRangeEO = NatRangeEE
    , kindFamilyEO = KindFamilyEE
    , rootRangeEO = RootRangeEE
    , childWindowEO = ChildWindowEE
    , childElemEO = ChildElemEE
    , widthNodeEO = WidthNodeEE
    }

opsDE :: ErrOps DecodeErr
opsDE =
  ErrOps
    { natRangeEO = NatRangeDE
    , kindFamilyEO = KindFamilyDE
    , rootRangeEO = RootRangeDE
    , childWindowEO = ChildWindowDE
    , childElemEO = ChildElemDE
    , widthNodeEO = WidthNodeDE
    }

validateCountsEE :: GreenFile -> Either EncodeErr ()
validateCountsEE (GreenFile arena0 _ _) = do
  let GreenArena nodes0 toks0 children0 = arena0
  _ <- countWord32EE "node count" (V.length nodes0)
  _ <- countWord32EE "token count" (V.length toks0)
  _ <- countWord32EE "child count" (V.length children0)
  pure ()

validateFileGeneric :: ErrOps e -> GreenFile -> Either e ()
validateFileGeneric ops0 (GreenFile arena0 root0 _) = do
  let
    GreenArena nodes0 toks0 _ = arena0

  rootW <- nat32Generic ops0 "root" root0
  when (fromIntegral rootW >= V.length nodes0) (Left (ops0.rootRangeEO root0 (V.length nodes0)))

  mapM_ (validateTokGeneric ops0) (V.toList toks0)
  mapM_ (uncurry (validateNodeGeneric ops0 arena0)) (zip [0 ..] (V.toList nodes0))

validateTokGeneric :: ErrOps e -> GreenTok -> Either e ()
validateTokGeneric ops0 (GreenTok kind0 width0 lexeme0 _ _) = do
  unless (isTokenKd kind0) (Left (ops0.kindFamilyEO "token kind" kind0))
  _ <- nat32Generic ops0 "token width" width0
  validateLexemeGeneric ops0 lexeme0

validateLexemeGeneric :: ErrOps e -> LexemeRef -> Either e ()
validateLexemeGeneric ops0 lexeme0 =
  case lexeme0 of
    ImplicitLR -> pure ()
    InternLR key0 -> do
      _ <- nat32Generic ops0 "text key" key0
      pure ()
    BlobLR key0 -> do
      _ <- nat32Generic ops0 "blob key" key0
      pure ()

validateNodeGeneric :: ErrOps e -> GreenArena -> Int -> GreenNode -> Either e ()
validateNodeGeneric ops0 arena0 nodeIx0 (GreenNode kind0 width0 firstChild0 childCount0 _ _) = do
  unless (isNodeKd kind0) (Left (ops0.kindFamilyEO "node kind" kind0))

  widthW <- nat32Generic ops0 "node width" width0
  expectW <- sumChildrenGeneric ops0 arena0 (nodeIdAt nodeIx0) firstChild0 childCount0
  when (widthW /= expectW) (Left (ops0.widthNodeEO (nodeIdAt nodeIx0) widthW expectW))


sumChildrenGeneric :: ErrOps e -> GreenArena -> GreenNodeId -> ChildIx -> Word32 -> Either e Word32
sumChildrenGeneric ops0 arena0@(GreenArena _ _ children0) nodeId0 firstChild0 childCount0 = do
  firstW <- nat32Generic ops0 "node firstChild" firstChild0
  let
    startI = fromIntegral firstW
    endI = startI + fromIntegral childCount0

  when (endI > V.length children0) (Left (ops0.childWindowEO nodeId0 firstChild0 childCount0 (V.length children0)))

  totalI <- foldM stepChildren 0 [startI .. endI - 1]
  if totalI > maxStoredI then
      Left (ops0.natRangeEO "node width sum" totalI)
  else
    pure (fromInteger totalI)
  where
  -- stepChildren :: Integer -> Int -> Either e Integer
  stepChildren acc0 childIx0 =
    let
      elem0 = children0 V.! childIx0
    in do
    validateElemGeneric ops0 arena0 childIx0 elem0
    widthW <- elemWidthGeneric ops0 arena0 elem0
    pure (acc0 + toInteger widthW)


validateElemGeneric :: ErrOps e -> GreenArena -> Int -> GreenElem -> Either e ()
validateElemGeneric ops0 (GreenArena nodes0 toks0 _) childIx0 elem0 =
  case elem0 of
    NodeGE nodeId0 -> do
      refW <- nat32Generic ops0 "child node ref" nodeId0
      when (fromIntegral refW >= V.length nodes0) (Left (ops0.childElemEO childIx0 elem0))
    TokGE tokId0 -> do
      refW <- nat32Generic ops0 "child token ref" tokId0
      when (fromIntegral refW >= V.length toks0) (Left (ops0.childElemEO childIx0 elem0))

elemWidthGeneric :: ErrOps e -> GreenArena -> GreenElem -> Either e Word32
elemWidthGeneric ops0 (GreenArena nodes0 toks0 _) elem0 =
  case elem0 of
    NodeGE nodeId0 -> do
      refW <- nat32Generic ops0 "child node ref" nodeId0
      let GreenNode _ width0 _ _ _ _ = nodes0 V.! fromIntegral refW
      nat32Generic ops0 "child node width" width0
    TokGE tokId0 -> do
      refW <- nat32Generic ops0 "child token ref" tokId0
      let GreenTok _ width0 _ _ _ = toks0 V.! fromIntegral refW
      nat32Generic ops0 "child token width" width0


nat32Generic :: Coercible a Int32 => ErrOps e -> Text -> a -> Either e Word32
nat32Generic ops0 label0 val0 =
  let
    n0 = toInteger (coerce val0 :: Int32)
  in
  if n0 < 0 then Left (ops0.natRangeEO label0 n0) else pure (fromInteger n0)

countWord32EE :: Text -> Int -> Either EncodeErr Word32
countWord32EE label0 n0
  | n0 < 0 = Left (CountRangeEE label0 n0)
  | toInteger n0 > maxStoredI = Left (CountRangeEE label0 n0)
  | otherwise = pure (fromIntegral n0)

countIntDE :: Text -> Word32 -> Either DecodeErr Int
countIntDE label0 n0
  | toInteger n0 > maxStoredI = Left (NatRangeDE label0 (toInteger n0))
  | otherwise = pure (fromIntegral n0)

decodeInt32DE :: Text -> Word32 -> Either DecodeErr Int32
decodeInt32DE label0 n0
  | toInteger n0 > maxStoredI = Left (NatRangeDE label0 (toInteger n0))
  | otherwise = pure (fromIntegral n0)

nodeIdAt :: Int -> GreenNodeId
nodeIdAt n0 = coerce (fromIntegral n0 :: Int32)

buildFile :: GreenFile -> BB.Builder
buildFile (GreenFile arena0 root0 hash0) =
  let GreenArena nodes0 toks0 children0 = arena0
  in BB.byteString magicCst
    <> buildVersion versionCurrent
    <> BB.word32LE (fromIntegral (V.length nodes0))
    <> BB.word32LE (fromIntegral (V.length toks0))
    <> BB.word32LE (fromIntegral (V.length children0))
    <> BB.word32LE (unsafeNat32 root0)
    <> BB.word64LE (coerce hash0 :: Word64)
    <> foldMap buildNode (V.toList nodes0)
    <> foldMap buildTok (V.toList toks0)
    <> foldMap buildElem (V.toList children0)

buildVersion :: VersionCst -> BB.Builder
buildVersion (VersionCst ver0) = BB.word16LE ver0

buildNode :: GreenNode -> BB.Builder
buildNode (GreenNode kind0 width0 firstChild0 childCount0 hash0 flags0) =
  BB.word16LE (kindTag kind0)
    <> BB.word32LE (unsafeNat32 width0)
    <> BB.word32LE (unsafeNat32 firstChild0)
    <> BB.word32LE childCount0
    <> BB.word64LE (coerce hash0 :: Word64)
    <> BB.word16LE (coerce flags0 :: Word16)

buildTok :: GreenTok -> BB.Builder
buildTok (GreenTok kind0 width0 lexeme0 origin0 flags0) =
  BB.word16LE (kindTag kind0)
    <> BB.word32LE (unsafeNat32 width0)
    <> buildLexeme lexeme0
    <> buildOrigin origin0
    <> BB.word16LE (coerce flags0 :: Word16)

buildLexeme :: LexemeRef -> BB.Builder
buildLexeme lexeme0 =
  case lexeme0 of
    ImplicitLR -> BB.word8 0 <> BB.word32LE 0
    InternLR key0 -> BB.word8 1 <> BB.word32LE (unsafeNat32 key0)
    BlobLR key0 -> BB.word8 2 <> BB.word32LE (unsafeNat32 key0)

buildOrigin :: TokenOrigin -> BB.Builder
buildOrigin origin0 =
  case origin0 of
    OriginalTO -> BB.word8 0 <> BB.word8 0
    SyntheticTO reason0 -> BB.word8 1 <> BB.word8 (tagSynthetic reason0)

buildElem :: GreenElem -> BB.Builder
buildElem elem0 =
  case elem0 of
    NodeGE nodeId0 -> BB.word8 0 <> BB.word32LE (unsafeNat32 nodeId0)
    TokGE tokId0 -> BB.word8 1 <> BB.word32LE (unsafeNat32 tokId0)

unsafeNat32 :: Coercible a Int32 => a -> Word32
unsafeNat32 val0 = fromIntegral (coerce val0 :: Int32)

tagSynthetic :: SyntheticReason -> Word8
tagSynthetic reason0 =
  case reason0 of
    LayoutSR -> 0
    RecoverySR -> 1
    GeneratedSR -> 2

getVersion :: Get VersionCst
getVersion = VersionCst <$> getWord16le

getHeader :: Get HeaderRaw
getHeader =
  HeaderRaw
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord64le

getBody :: Int -> Int -> Int -> Get BodyRaw
getBody nodeCount0 tokCount0 childCount0 = do
  nodes0 <- replicateM nodeCount0 getNode
  toks0 <- replicateM tokCount0 getTok
  children0 <- replicateM childCount0 getElem
  pure BodyRaw { nodesBR = nodes0, toksBR = toks0, childrenBR = children0 }

getNode :: Get NodeRaw
getNode =
  NodeRaw
    <$> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord64le
    <*> getWord16le

getTok :: Get TokRaw
getTok =
  TokRaw
    <$> getWord16le
    <*> getWord32le
    <*> getWord8
    <*> getWord32le
    <*> getWord8
    <*> getWord8
    <*> getWord16le

getElem :: Get ElemRaw
getElem = ElemRaw <$> getWord8 <*> getWord32le

runStep :: Get a -> ByteString -> Either DecodeErr (a, ByteString)
runStep get0 bytes0 =
  case runGetOrFail get0 (BL.fromStrict bytes0) of
    Left (_, _, msg0) -> Left (GetFailDE (T.pack msg0))
    Right (rest0, _, val0) -> Right (val0, BL.toStrict rest0)

buildFileDE :: HeaderRaw -> BodyRaw -> Either DecodeErr GreenFile
buildFileDE head0 body0 = do
  root0 <- coerce <$> decodeInt32DE "root" head0.rootHR
  nodes0 <- V.fromList <$> mapM decodeNodeDE body0.nodesBR
  toks0 <- V.fromList <$> mapM decodeTokDE body0.toksBR
  children0 <- V.fromList <$> mapM decodeElemDE body0.childrenBR

  let arena0 = GreenArena nodes0 toks0 children0
  let file0 = GreenFile arena0 root0 (coerce head0.sourceHashHR)

  validateFileGeneric opsDE file0
  pure file0

decodeNodeDE :: NodeRaw -> Either DecodeErr GreenNode
decodeNodeDE raw0 = do
  kind0 <- decodeNodeKindDE raw0.kindRN
  width0 <- decodeInt32DE "node width" raw0.widthRN
  firstChild0 <- decodeInt32DE "node firstChild" raw0.firstChildRN
  pure
    ( GreenNode
        kind0
        (coerce width0)
        (coerce firstChild0)
        raw0.childCountRN
        (coerce raw0.hashRN)
        (coerce raw0.flagsRN)
    )

decodeTokDE :: TokRaw -> Either DecodeErr GreenTok
decodeTokDE raw0 = do
  kind0 <- decodeTokKindDE raw0.kindRT
  width0 <- decodeInt32DE "token width" raw0.widthRT
  lexeme0 <- decodeLexemeDE raw0.lexTagRT raw0.lexPayloadRT
  origin0 <- decodeOriginDE raw0.originTagRT raw0.originPayloadRT
  pure
    ( GreenTok
        kind0
        (coerce width0)
        lexeme0
        origin0
        (coerce raw0.flagsRT)
    )

decodeElemDE :: ElemRaw -> Either DecodeErr GreenElem
decodeElemDE raw0 =
  case raw0.tagRE of
    0 -> NodeGE <$> (coerce <$> decodeInt32DE "child node ref" raw0.payloadRE)
    1 -> TokGE <$> (coerce <$> decodeInt32DE "child token ref" raw0.payloadRE)
    tag0 -> Left (TagInvalidDE "child element tag" (fromIntegral tag0))

decodeLexemeDE :: Word8 -> Word32 -> Either DecodeErr LexemeRef
decodeLexemeDE tag0 payload0 =
  case tag0 of
    0 ->
      if payload0 == 0
        then pure ImplicitLR
        else Left (TagInvalidDE "implicit lexeme payload" (fromIntegral payload0))
    1 -> InternLR <$> (coerce <$> decodeInt32DE "text key" payload0)
    2 -> BlobLR <$> (coerce <$> decodeInt32DE "blob key" payload0)
    _ -> Left (TagInvalidDE "lexeme tag" (fromIntegral tag0))

decodeOriginDE :: Word8 -> Word8 -> Either DecodeErr TokenOrigin
decodeOriginDE tag0 payload0 =
  case tag0 of
    0 ->
      if payload0 == 0
        then pure OriginalTO
        else Left (TagInvalidDE "original token payload" (fromIntegral payload0))
    1 -> SyntheticTO <$> decodeSyntheticDE payload0
    _ -> Left (TagInvalidDE "token origin tag" (fromIntegral tag0))

decodeSyntheticDE :: Word8 -> Either DecodeErr SyntheticReason
decodeSyntheticDE tag0 =
  case tag0 of
    0 -> pure LayoutSR
    1 -> pure RecoverySR
    2 -> pure GeneratedSR
    _ -> Left (TagInvalidDE "synthetic reason tag" (fromIntegral tag0))

decodeNodeKindDE :: Word16 -> Either DecodeErr SyntaxKind
decodeNodeKindDE tag0 = do
  kind0 <- decodeKindDE "node kind tag" tag0
  if isNodeKd kind0
    then pure kind0
    else Left (KindFamilyDE "node kind" kind0)

decodeTokKindDE :: Word16 -> Either DecodeErr SyntaxKind
decodeTokKindDE tag0 = do
  kind0 <- decodeKindDE "token kind tag" tag0
  if isTokenKd kind0
    then pure kind0
    else Left (KindFamilyDE "token kind" kind0)

decodeKindDE :: Text -> Word16 -> Either DecodeErr SyntaxKind
decodeKindDE label0 tag0 =
  case kindFromTagMay tag0 of
    Just kind0 -> pure kind0
    Nothing -> Left (KindTagDE label0 tag0)

kindFromTagMay :: Word16 -> Maybe SyntaxKind
kindFromTagMay tag0
  | tag0 > maxKindTag = Nothing
  | otherwise = Just (toEnum (fromIntegral tag0))

maxKindTag :: Word16
maxKindTag = kindTag maxBound