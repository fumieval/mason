{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#define LIB Data.ByteString.FastBuilder
#define NAME Fast
#include "template.hs"

encodeUtf8Builder :: T.Text -> Builder
encodeUtf8Builder = byteString . T.encodeUtf8
