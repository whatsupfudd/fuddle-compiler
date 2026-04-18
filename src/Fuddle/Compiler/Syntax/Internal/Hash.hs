{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module Fuddle.Compiler.Syntax.Internal.Hash
  ( ElemHash(..)
  , hashTok
  , hashNode
  , hashNodeCount
  , hashElem
  ) where

import Data.Bits (rotateL, shiftR, xor)
import Data.Foldable (foldl')
import Data.Word (Word16, Word32, Word64)
import Fuddle.Compiler.Base.Core (Hash64(..), TextSize(..))
import Fuddle.Compiler.Syntax.Green (NodeFlags(..))
import Fuddle.Compiler.Syntax.Kind (SyntaxKind, kindTag)
import Fuddle.Compiler.Syntax.Token
  ( BlobKey(..)
  , LexemeRef(..)
  , SyntheticReason(..)
  , TextKey(..)
  , TokenFlags(..)
  , TokenOrigin(..)
  )

-- Deterministic structural hashing for green-tree subtrees.
-- This is intentionally non-cryptographic and is meant for:
--   * subtree identity / reuse
--   * cache keys
--   * deterministic serialized CSTs
--
-- Stability depends on deterministic lexer/interner assignment for TextKey/BlobKey.

data ElemHash
  = NodeEH !Hash64
  | TokEH !Hash64
  deriving stock (Eq, Show)

newtype AccHash = AccHash Word64
  deriving stock (Eq, Show)

hashTok :: SyntaxKind -> TextSize -> LexemeRef -> TokenOrigin -> TokenFlags -> Hash64
hashTok kind width lexRef origin flags =
  doneAcc
    ( putTokenFlags flags
    $ putTokenOrigin origin
    $ putLexemeRef lexRef
    $ putTextSize width
    $ putKind kind
    $ putTag tagTokH
    $ initAcc seedTokH
    )

hashNode :: Foldable f => SyntaxKind -> TextSize -> NodeFlags -> f ElemHash -> Hash64
hashNode kind width flags elems =
  let !base = initNodeAcc kind width flags
      (!count, !acc) = foldl' stepNode (0, base) elems
  in doneAcc (putWord32 count acc)

hashNodeCount :: Foldable f => SyntaxKind -> TextSize -> Word32 -> NodeFlags -> f ElemHash -> Hash64
hashNodeCount kind width count flags elems =
  let !base = initNodeAcc kind width flags
      !acc = foldl' stepElem base elems
  in doneAcc (putWord32 count acc)

hashElem :: ElemHash -> Hash64
hashElem elemHash = doneAcc (putElemHash elemHash (initAcc seedElemH))

initNodeAcc :: SyntaxKind -> TextSize -> NodeFlags -> AccHash
initNodeAcc kind width flags =
  putNodeFlags flags
    $ putTextSize width
    $ putKind kind
    $ putTag tagNodeH
    $ initAcc seedNodeH

stepNode :: (Word32, AccHash) -> ElemHash -> (Word32, AccHash)
stepNode (!count, !acc) elemHash = (count + 1, stepElem acc elemHash)

stepElem :: AccHash -> ElemHash -> AccHash
stepElem !acc elemHash = putElemHash elemHash acc

putElemHash :: ElemHash -> AccHash -> AccHash
putElemHash elemHash acc =
  case elemHash of
    NodeEH subH -> putHash subH (putTag tagElemNodeH acc)
    TokEH subH -> putHash subH (putTag tagElemTokH acc)

putKind :: SyntaxKind -> AccHash -> AccHash
putKind kind = putWord16 (kindTag kind)

putTextSize :: TextSize -> AccHash -> AccHash
putTextSize (TextSize n) = putWord32 (fromIntegral n)

putTextKey :: TextKey -> AccHash -> AccHash
putTextKey (TextKey n) = putWord32 (fromIntegral n)

putBlobKey :: BlobKey -> AccHash -> AccHash
putBlobKey (BlobKey n) = putWord32 (fromIntegral n)

putLexemeRef :: LexemeRef -> AccHash -> AccHash
putLexemeRef lexRef acc =
  case lexRef of
    ImplicitLR -> putTag tagLexImplicitH acc
    InternLR key -> putTextKey key (putTag tagLexInternH acc)
    BlobLR key -> putBlobKey key (putTag tagLexBlobH acc)

putTokenOrigin :: TokenOrigin -> AccHash -> AccHash
putTokenOrigin origin acc =
  case origin of
    OriginalTO -> putTag tagOriginOriginalH acc
    SyntheticTO reason -> putSyntheticReason reason (putTag tagOriginSyntheticH acc)

putSyntheticReason :: SyntheticReason -> AccHash -> AccHash
putSyntheticReason reason acc =
  case reason of
    LayoutSR -> putTag tagReasonLayoutH acc
    RecoverySR -> putTag tagReasonRecoveryH acc
    GeneratedSR -> putTag tagReasonGeneratedH acc

putTokenFlags :: TokenFlags -> AccHash -> AccHash
putTokenFlags (TokenFlags bits) = putWord16 bits

putNodeFlags :: NodeFlags -> AccHash -> AccHash
putNodeFlags (NodeFlags bits) = putWord16 bits

putHash :: Hash64 -> AccHash -> AccHash
putHash (Hash64 w) = putWord64 w

putWord16 :: Word16 -> AccHash -> AccHash
putWord16 w = putWord64 (fromIntegral w)

putWord32 :: Word32 -> AccHash -> AccHash
putWord32 w = putWord64 (fromIntegral w)

putWord64 :: Word64 -> AccHash -> AccHash
putWord64 w = stepAcc w

putTag :: Word64 -> AccHash -> AccHash
putTag = stepAcc

initAcc :: Word64 -> AccHash
initAcc = AccHash

doneAcc :: AccHash -> Hash64
doneAcc (AccHash w) = Hash64 (mix64 (w `xor` finalSaltH))

stepAcc :: Word64 -> AccHash -> AccHash
stepAcc !w (AccHash !acc) =
  let !x0 = w + combineSaltH + rotateL acc 19 + (acc `shiftR` 3)
      !x1 = acc `xor` mix64 x0
      !x2 = x1 * stepMulH + stepSaltH
  in AccHash x2

mix64 :: Word64 -> Word64
mix64 !w =
  let !x0 = (w `xor` (w `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !x1 = (x0 `xor` (x0 `shiftR` 27)) * 0x94d049bb133111eb
  in x1 `xor` (x1 `shiftR` 31)

seedTokH :: Word64
seedTokH = 0x243f6a8885a308d3

seedNodeH :: Word64
seedNodeH = 0x13198a2e03707344

seedElemH :: Word64
seedElemH = 0xa4093822299f31d0

combineSaltH :: Word64
combineSaltH = 0x9e3779b97f4a7c15

stepMulH :: Word64
stepMulH = 0xbf58476d1ce4e5b9

stepSaltH :: Word64
stepSaltH = 0x94d049bb133111eb

finalSaltH :: Word64
finalSaltH = 0x27d4eb2f165667c5

tagTokH :: Word64
tagTokH = 0x746f6b01

tagNodeH :: Word64
tagNodeH = 0x6e6f646501

tagElemNodeH :: Word64
tagElemNodeH = 0x656c6d6e

tagElemTokH :: Word64
tagElemTokH = 0x656c6d74

tagLexImplicitH :: Word64
tagLexImplicitH = 0x6c657800

tagLexInternH :: Word64
tagLexInternH = 0x6c657801

tagLexBlobH :: Word64
tagLexBlobH = 0x6c657802

tagOriginOriginalH :: Word64
tagOriginOriginalH = 0x6f726700

tagOriginSyntheticH :: Word64
tagOriginSyntheticH = 0x6f726701

tagReasonLayoutH :: Word64
tagReasonLayoutH = 0x72736e10

tagReasonRecoveryH :: Word64
tagReasonRecoveryH = 0x72736e11

tagReasonGeneratedH :: Word64
tagReasonGeneratedH = 0x72736e12