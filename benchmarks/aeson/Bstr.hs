{-# LANGUAGE CPP #-}
#define LIB Data.ByteString.Builder
#define NAME Bstr
#include "template.hs"

rebuild :: Builder -> Builder
rebuild = id

toStrictByteString = L.toStrict . toLazyByteString

encodeUtf8Builder :: T.Text -> Builder
encodeUtf8Builder = T.encodeUtf8Builder
