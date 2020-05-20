----------------------------------------------------------------------------
-- |
-- Module      :  Mason.Builder.Compat
-- Copyright   :  (c) Fumiaki Kinoshita 2020-
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- API which is almost compatible with Data.ByteString.Builder
----------------------------------------------------------------------------
module Mason.Builder.Compat
  ( Builder
  -- * Runners
  , D.toStrictByteString
  , D.toLazyByteString
  , D.hPutBuilderLen
  , D.hPutBuilder
  , D.sendBuilder
  -- * Primitives
  , flush
  -- * Bytes
  , byteString
  , lazyByteString
  , shortByteString
  -- * Text
  , textUtf8
  , encodeUtf8Builder
  , encodeUtf8BuilderEscaped
  , char7
  , string7
  , char8
  , string8
  , charUtf8
  , stringUtf8
  -- * Primitive
  , storable
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
  -- * Numeral
  , floatDec
  , doubleDec
  , doubleSI
  , doubleExp
  , doubleFixed
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
  ) where
import Mason.Builder hiding (Builder)
import Mason.Builder.Dynamic as D

type Builder = D.DynBuilder