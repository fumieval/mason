{-# LANGUAGE RankNTypes #-}
import qualified Data.ByteString.Builder as B
import qualified Mason.Builder.Dynamic as MD
import qualified Mason.Builder as M
import qualified Data.ByteString.FastBuilder as F
import Test.Tasty.Bench

b_bytestring :: Int -> B.Builder
b_bytestring 0 = B.char7 'o'
b_bytestring 1 = B.char7 'i'
b_bytestring n = b_bytestring (n-1) <> B.char7 '-' <> b_bytestring (n-2)

b_mason :: Int -> MD.DynBuilder
b_mason 0 = M.char7 'o'
b_mason 1 = M.char7 'i'
b_mason n = b_mason (n-1) <> M.char7 '-' <> b_mason (n-2)

b_fast :: Int -> F.Builder
b_fast 0 = F.char7 'o'
b_fast 1 = F.char7 'i'
b_fast n = b_fast (n-1) <> F.char7 '-' <> b_fast (n-2)

main = defaultMain
  [ bench "mason" $ nf (MD.toStrictByteString . b_mason) 10
  , bench "fast-builder" $ nf (F.toStrictByteString . b_fast) 10
  , bench "bytestring" $ nf (B.toLazyByteString . b_bytestring) 10
  ]
