{-# LANGUAGE MagicHash, CPP, UnboxedTuples #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Mason.Builders
-- Copyright   :  (c) Fumiaki Kinoshita 2019
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
----------------------------------------------------------------------------
module Mason.Builder
  ( Builder
  , BuilderFor
  , Buildable
  -- * Runners
  , toStrictByteString
  , toLazyByteString
  , hPutBuilderLen
  , hPutBuilder
  , sendBuilder
  -- * Raw
  , flush
  , encodeUtf8Builder
  , encodeUtf8BuilderEscaped
  , byteString
  , lazyByteString
  , shortByteString
  , int8
  , word8
  , int16LE
  , int32LE
  , int64LE
  , word16LE
  , word32LE
  , word64LE
  , floatLE
  , doubleLE
  , int16BE
  , int32BE
  , int64BE
  , word16BE
  , word32BE
  , word64BE
  , floatBE
  , doubleBE
  , char7
  , string7
  , char8
  , string8
  , charUtf8
  , stringUtf8
  , floatDec
  , doubleDec
  , word8Dec
  , word16Dec
  , word32Dec
  , word64Dec
  , wordDec
  , int8Dec
  , int16Dec
  , int32Dec
  , int64Dec
  , intDec
  , integerDec
  , word8Hex
  , word16Hex
  , word32Hex
  , word64Hex
  , wordHex
  , int8HexFixed
  , int16HexFixed
  , int32HexFixed
  , int64HexFixed
  , word8HexFixed
  , word16HexFixed
  , word32HexFixed
  , word64HexFixed
  , floatHexFixed
  , doubleHexFixed
  , byteStringHex
  , lazyByteStringHex
  -- * Advanced
  , primFixed
  , primBounded
  , lengthPrefixedWithin
  ) where

import Control.Monad
import Data.Bits
import Data.Word
import Data.Int
import qualified Data.Text as T
import Foreign.C.Types
import Foreign.Ptr (Ptr, plusPtr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Mason.Builder.Internal as B
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as P
import GHC.Integer.GMP.Internals
import GHC.Types (Int(..))
import System.IO (Handle)

-- | Put the content of a 'Builder' to a 'Handle'.
hPutBuilder :: Handle -> BuilderFor PutBuilderEnv -> IO ()
hPutBuilder h b = void $ hPutBuilderLen h b
{-# INLINE hPutBuilder #-}

-- | Combine chunks of a lazy 'BL.ByteString'
lazyByteString :: BL.ByteString -> Builder
lazyByteString = foldMap byteString . BL.toChunks
{-# INLINE lazyByteString #-}

------------------------------------------------------------------------------
-- Binary encodings
------------------------------------------------------------------------------

-- | Encode a single signed byte as-is.
--
{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 = B.primFixed P.int8

-- | Encode a single unsigned byte as-is.
--
{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 = B.primFixed P.word8


------------------------------------------------------------------------------
-- Binary little-endian encodings
------------------------------------------------------------------------------

-- | Encode an 'Int16' in little endian format.
{-# INLINE int16LE #-}
int16LE :: Int16 -> Builder
int16LE = B.primFixed P.int16LE

-- | Encode an 'Int32' in little endian format.
{-# INLINE int32LE #-}
int32LE :: Int32 -> Builder
int32LE = B.primFixed P.int32LE

-- | Encode an 'Int64' in little endian format.
{-# INLINE int64LE #-}
int64LE :: Int64 -> Builder
int64LE = B.primFixed P.int64LE

-- | Encode a 'Word16' in little endian format.
{-# INLINE word16LE #-}
word16LE :: Word16 -> Builder
word16LE = B.primFixed P.word16LE

-- | Encode a 'Word32' in little endian format.
{-# INLINE word32LE #-}
word32LE :: Word32 -> Builder
word32LE = B.primFixed P.word32LE

-- | Encode a 'Word64' in little endian format.
{-# INLINE word64LE #-}
word64LE :: Word64 -> Builder
word64LE = B.primFixed P.word64LE

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: Float -> Builder
floatLE = B.primFixed P.floatLE

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: Double -> Builder
doubleLE = B.primFixed P.doubleLE


------------------------------------------------------------------------------
-- Binary big-endian encodings
------------------------------------------------------------------------------

-- | Encode an 'Int16' in big endian format.
{-# INLINE int16BE #-}
int16BE :: Int16 -> Builder
int16BE = B.primFixed P.int16BE

-- | Encode an 'Int32' in big endian format.
{-# INLINE int32BE #-}
int32BE :: Int32 -> Builder
int32BE = B.primFixed P.int32BE

-- | Encode an 'Int64' in big endian format.
{-# INLINE int64BE #-}
int64BE :: Int64 -> Builder
int64BE = B.primFixed P.int64BE

-- | Encode a 'Word16' in big endian format.
{-# INLINE word16BE #-}
word16BE :: Word16 -> Builder
word16BE = B.primFixed P.word16BE

-- | Encode a 'Word32' in big endian format.
{-# INLINE word32BE #-}
word32BE :: Word32 -> Builder
word32BE = B.primFixed P.word32BE

-- | Encode a 'Word64' in big endian format.
{-# INLINE word64BE #-}
word64BE :: Word64 -> Builder
word64BE = B.primFixed P.word64BE

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: Float -> Builder
floatBE = B.primFixed P.floatBE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: Double -> Builder
doubleBE = B.primFixed P.doubleBE

------------------------------------------------------------------------------
-- ASCII encoding
------------------------------------------------------------------------------

-- | Char7 encode a 'Char'.
{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 = B.primFixed P.char7

-- | Char7 encode a 'String'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = B.primMapListFixed P.char7

------------------------------------------------------------------------------
-- ISO/IEC 8859-1 encoding
------------------------------------------------------------------------------

-- | Char8 encode a 'Char'.
{-# INLINE char8 #-}
char8 :: Char -> Builder
char8 = B.primFixed P.char8

-- | Char8 encode a 'String'.
{-# INLINE string8 #-}
string8 :: String -> Builder
string8 = B.primMapListFixed P.char8

------------------------------------------------------------------------------
-- UTF-8 encoding
------------------------------------------------------------------------------

-- | UTF-8 encode a 'Char'.
{-# INLINE charUtf8 #-}
charUtf8 :: Char -> Builder
charUtf8 = B.primBounded P.charUtf8

-- | Encode 'T.Text' as a UTF-8 byte stream.
encodeUtf8Builder :: T.Text -> Builder
encodeUtf8Builder = B.encodeUtf8BuilderEscaped (P.liftFixedToBounded P.word8)
{-# INLINE encodeUtf8Builder #-}

--------------------
-- Unsigned integers
--------------------

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE word8Dec #-}
word8Dec :: Word8 -> Builder
word8Dec = B.primBounded P.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE word16Dec #-}
word16Dec :: Word16 -> Builder
word16Dec = B.primBounded P.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE word32Dec #-}
word32Dec :: Word32 -> Builder
word32Dec = B.primBounded P.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Builder
word64Dec = B.primBounded P.word64Dec

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE wordDec #-}
wordDec :: Word -> Builder
wordDec = B.primBounded P.wordDec

-- Floating point numbers
-------------------------

-- | /Currently slow./ Decimal encoding of an IEEE 'Float'.
{-# INLINE floatDec #-}
floatDec :: Float -> Builder
floatDec = string7 . show

-- | Decimal encoding of an IEEE 'Double'.
{-# INLINE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec x
  | isNaN x = string7 "NaN"
  | isInfinite x = if x < 0 then string7 "-Infinity" else string7 "Infinity"
  | x < 0 = char7 '-' <> grisu (-x)
  | isNegativeZero x = string7 "-0.0"
  | x == 0 = string7 "0.0"
  | otherwise = grisu x
  where
    grisu v = B.ensure 24 $ \(B.Buffer end ptr) -> do
      n <- dtoa_grisu3 v ptr
      return $ B.Buffer end $ plusPtr ptr (fromIntegral n)

foreign import ccall unsafe "static dtoa_grisu3"
  dtoa_grisu3 :: Double -> Ptr Word8 -> IO CInt

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

-- | Decimal encoding of an 'Int8' using the ASCII digits.
--
-- e.g.
--
-- > toLazyByteString (int8Dec 42)   = "42"
-- > toLazyByteString (int8Dec (-1)) = "-1"
--
{-# INLINE int8Dec #-}
int8Dec :: Int8 -> Builder
int8Dec = B.primBounded P.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE int16Dec #-}
int16Dec :: Int16 -> Builder
int16Dec = B.primBounded P.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE int32Dec #-}
int32Dec :: Int32 -> Builder
int32Dec = B.primBounded P.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Builder
int64Dec = B.primBounded P.int64Dec

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec = B.primBounded P.intDec



------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

-- | Shortest hexadecimal encoding of a 'Word8' using lower-case characters.
{-# INLINE word8Hex #-}
word8Hex :: Word8 -> Builder
word8Hex = B.primBounded P.word8Hex

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE word16Hex #-}
word16Hex :: Word16 -> Builder
word16Hex = B.primBounded P.word16Hex

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE word32Hex #-}
word32Hex :: Word32 -> Builder
word32Hex = B.primBounded P.word32Hex

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
{-# INLINE word64Hex #-}
word64Hex :: Word64 -> Builder
word64Hex = B.primBounded P.word64Hex

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE wordHex #-}
wordHex :: Word -> Builder
wordHex = B.primBounded P.wordHex

-- fixed width; leading zeroes
------------------------------

-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: Int8 -> Builder
int8HexFixed = B.primFixed P.int8HexFixed

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Int16 -> Builder
int16HexFixed = B.primFixed P.int16HexFixed

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Int32 -> Builder
int32HexFixed = B.primFixed P.int32HexFixed

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Int64 -> Builder
int64HexFixed = B.primFixed P.int64HexFixed

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: Word8 -> Builder
word8HexFixed = B.primFixed P.word8HexFixed

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Word16 -> Builder
word16HexFixed = B.primFixed P.word16HexFixed

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Word32 -> Builder
word32HexFixed = B.primFixed P.word32HexFixed

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Word64 -> Builder
word64HexFixed = B.primFixed P.word64HexFixed

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Float -> Builder
floatHexFixed = B.primFixed P.floatHexFixed

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Double -> Builder
doubleHexFixed = B.primFixed P.doubleHexFixed

-- | Encode each byte of a 'S.ByteString' using its fixed-width hex encoding.
{-# NOINLINE byteStringHex #-} -- share code
byteStringHex :: B.ByteString -> Builder
byteStringHex = B.primMapByteStringFixed P.word8HexFixed

-- | Encode each byte of a lazy 'L.ByteString' using its fixed-width hex encoding.
{-# NOINLINE lazyByteStringHex #-} -- share code
lazyByteStringHex :: BL.ByteString -> Builder
lazyByteStringHex = B.primMapLazyByteStringFixed P.word8HexFixed

#define PAIR(a,b) (# a,b #)

-- | Select an implementation depending on the bit-size of 'Word's.
-- Currently, it produces a runtime failure if the bitsize is different.
-- This is detected by the testsuite.
{-# INLINE caseWordSize_32_64 #-}
caseWordSize_32_64 :: a -- Value to use for 32-bit 'Word's
                   -> a -- Value to use for 64-bit 'Word's
                   -> a
caseWordSize_32_64 f32 f64 =
#if MIN_VERSION_base(4,7,0)
  case finiteBitSize (undefined :: Word) of
#else
  case bitSize (undefined :: Word) of
#endif
    32 -> f32
    64 -> f64
    s  -> error $ "caseWordSize_32_64: unsupported Word bit-size " ++ show s

maxPow10 :: Integer
maxPow10 = toInteger $ (10 :: Int) ^ caseWordSize_32_64 (9 :: Int) 18

-- | Decimal encoding of an 'Integer' using the ASCII digits.
-- Simon Meier's improved implementation from https://github.com/haskell/bytestring/commit/92f19a5d94761042b44a433d7331107611e4d717
integerDec :: Integer -> Builder
integerDec (S# i#) = intDec (I# i#)
integerDec i
    | i < 0     = B.primFixed P.char8 '-' `mappend` go (-i)
    | otherwise =                                   go ( i)
  where
    errImpossible fun =
        error $ "integerDec: " ++ fun ++ ": the impossible happened."

    go :: Integer -> Builder
    go n | n < maxPow10 = intDec (fromInteger n)
         | otherwise    =
             case putH (splitf (maxPow10 * maxPow10) n) of
               (x:xs) -> intDec x `mappend` B.primMapListBounded intDecPadded xs
               []     -> errImpossible "integerDec: go"

    splitf :: Integer -> Integer -> [Integer]
    splitf pow10 n0
      | pow10 > n0  = [n0]
      | otherwise   = splith (splitf (pow10 * pow10) n0)
      where
        splith []     = errImpossible "splith"
        splith (n:ns) =
            case n `quotRemInteger` pow10 of
                PAIR(q,r) | q > 0     -> q : r : splitb ns
                          | otherwise ->     r : splitb ns

        splitb []     = []
        splitb (n:ns) = case n `quotRemInteger` pow10 of
                            PAIR(q,r) -> q : r : splitb ns

    putH :: [Integer] -> [Int]
    putH []     = errImpossible "putH"
    putH (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(x,y)
                        | q > 0     -> q : r : putB ns
                        | otherwise ->     r : putB ns
                        where q = fromInteger x
                              r = fromInteger y

    putB :: [Integer] -> [Int]
    putB []     = []
    putB (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(q,r) -> fromInteger q : fromInteger r : putB ns

foreign import ccall unsafe "static _hs_bytestring_int_dec_padded9"
    c_int_dec_padded9 :: CInt -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static _hs_bytestring_long_long_int_dec_padded18"
    c_long_long_int_dec_padded18 :: CLLong -> Ptr Word8 -> IO ()

{-# INLINE intDecPadded #-}
intDecPadded :: P.BoundedPrim Int
intDecPadded = P.liftFixedToBounded $ caseWordSize_32_64
    (P.fixedPrim  9 $ c_int_dec_padded9            . fromIntegral)
    (P.fixedPrim 18 $ c_long_long_int_dec_padded18 . fromIntegral)
