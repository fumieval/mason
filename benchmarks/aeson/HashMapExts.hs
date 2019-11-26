{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module HashMapExts where

import Data.HashMap.Strict (HashMap)
import Data.Monoid
import GHC.Exts (SmallArray#, sizeofSmallArray#, indexSmallArray#, Int(..))
import Unsafe.TrueName (truename)

foldMapWithKey :: (Monoid m) => (k -> a -> m) -> HashMap k a -> m
foldMapWithKey f = go
  where
    go hm = case hm of
      [truename| ''HashMap Empty |] -> mempty
      [truename| ''HashMap BitmapIndexed | _ ary |] -> foldMapArray go ary
      [truename| ''HashMap Full | ary |] -> foldMapArray go ary
      [truename| ''HashMap Collision | _ ary |] -> foldMapArray leaf ary
      [truename| ''HashMap Leaf | _ l |] -> leaf l

    leaf [truename| ''HashMap Leaf Leaf L | k v|]  = f k v
{-# INLINE foldMapWithKey #-}

foldMapArray :: (Monoid m) =>
    (a -> m) -> [truename| ''HashMap Full Array |] a -> m
foldMapArray f [truename| ''HashMap Full Array Array | a |] = foldMapArray' f a

foldMapArray' :: (Monoid m) => (a -> m) -> SmallArray# a -> m
foldMapArray' f arr = go 0
  where
    go k@(I# k#)
      | k >= I# (sizeofSmallArray# arr) = mempty
      | otherwise = case indexSmallArray# arr k# of
        (# v #) -> f v <> go (k + 1)
{-# INLINE foldMapArray' #-}
