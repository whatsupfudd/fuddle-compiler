{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuddle.Compiler.Base.Core
  ( TextSize(..)
  , Hash64(..)
  , zeroSize
  , plusSize
  , minusSizeMay
  , maxSize
  , minSize
  , fromIntSize
  , toIntSize
  ) where

import Data.Int (Int32)
import Data.Word (Word64)

newtype TextSize = TextSize Int32
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

newtype Hash64 = Hash64 Word64
  deriving stock (Eq, Ord, Show)

zeroSize :: TextSize
zeroSize = TextSize 0

plusSize :: TextSize -> TextSize -> TextSize
plusSize lhs rhs = mkSizeChecked "plusSize" (toInteger lhs + toInteger rhs)

minusSizeMay :: TextSize -> TextSize -> Maybe TextSize
minusSizeMay lhs rhs
  | lhs < rhs = Nothing
  | otherwise = Just (mkSizeChecked "minusSizeMay" (toInteger lhs - toInteger rhs))

maxSize :: TextSize -> TextSize -> TextSize
maxSize = max

minSize :: TextSize -> TextSize -> TextSize
minSize = min

fromIntSize :: Int -> TextSize
fromIntSize n = mkSizeChecked "fromIntSize" (toInteger n)

toIntSize :: TextSize -> Int
toIntSize (TextSize n) = fromIntegral n

mkSizeChecked :: String -> Integer -> TextSize
mkSizeChecked fun n
  | n < 0 = error (errSize fun ("negative value: " <> show n))
  | n > maxSizeI = error (errSize fun ("value exceeds Int32 range: " <> show n))
  | otherwise = TextSize (fromInteger n)

maxSizeI :: Integer
maxSizeI = toInteger (maxBound :: Int32)

errSize :: String -> String -> String
errSize fun msg = "Fuddle.Compiler.Base.Core." <> fun <> ": " <> msg