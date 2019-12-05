{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module NAME
  ( fromValue
  , toStrictByteString
  , valueToByteString
  , valueToLazyByteString
  , putValue
  , literal
  , doubleDec
  ) where

#ifndef LIB
#define LIB Data.ByteString.FastBuilder
#endif

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import System.IO (Handle)

import LIB
import HashMapExts

literal :: Builder
literal = "Haskell！Haskell！Haskell！Haskellぅぅうううわぁあああああああああん！！！あぁ…ああ…あっあっー！あぁあああ！！！HaskellHaskellHaskellぅううぁわぁああああ！！"

putValue :: Handle -> Value -> IO ()
putValue h v = hPutBuilder h $ fromValue v

valueToLazyByteString :: Value -> L.ByteString
valueToLazyByteString = toLazyByteString . fromValue

valueToByteString :: Value -> B.ByteString
valueToByteString = toStrictByteString . fromValue

fromValue :: Value -> Builder
fromValue = go0 where
  go0 = rebuild . go
  go (Object obj) = char8 '{' <> foldMapWithKey f obj <> char8 '}'
    where
        f k v =
          encodeUtf8Builder k <> char8 ':' <> go0 v
          <> char8 ','
  go (Array arr) = V.foldr f (const $ char8 ']') arr True
    where
      f x r initial =
        (if initial then char8 '[' else char8 ',')
        <> go0 x <> r False
  go (String s) = encodeUtf8Builder s
  go (Number n) = either doubleDec integerDec $ Sci.floatingOrInteger n
  go (Bool False) = "false"
  go (Bool True) = "true"
  go Null = "null"
{-# INLINE fromValue #-}
