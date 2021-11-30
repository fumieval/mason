{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#define LIB Mason.Builder.Compat hiding (toStrictByteString, toLazyByteString, hPutBuilder)
#define LIB_EXTRA Mason.Builder.Dynamic
#define NAME Mason
#include "template.hs"

rebuild = id
