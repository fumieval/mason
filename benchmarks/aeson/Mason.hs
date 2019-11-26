{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#define LIB Mason.Builder
#define NAME Mason
#include "template.hs"

rebuild = id
