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

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as HM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import System.IO (Handle)

import LIB
#ifdef LIB_EXTRA
import LIB_EXTRA
#endif

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
  go (Object obj) = char8 '{' <> HM.foldMapWithKey f obj <> char8 '}'
    where
        f k v =
          encodeUtf8Builder (Key.toText k) <> char8 ':' <> go0 v
          <> char8 ','
  go (Array arr) = V.foldr f (const $ char8 ']') arr True
    where
      f x r initial =
        (if initial then char8 '[' else char8 ',')
        <> go0 x <> r False
  go (String s) = encodeUtf8Builder s
  go (Number n) = case Sci.floatingOrInteger n of
    Left x -> doubleDec x
    Right x -> integerDec x
  go (Bool False) = "false"
  go (Bool True) = "true"
  go Null = "null"
{-# INLINE fromValue #-}
