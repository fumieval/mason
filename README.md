mason: alacritous builder library
====

[![Build Status](https://travis-ci.com/fumieval/mason.svg?branch=master)](https://travis-ci.com/fumieval/mason)
[![Hackage](https://img.shields.io/hackage/v/mason)](https://hackage.haskell.org/package/mason)

mason is a builder & IO library.

* __Fast__: much faster than bytestring's Builder.
* __Extensible__: Builders can be consumed in a user-defined way.
* __Hackable__: Low-level APIs are exposed. It's easy to plug in even pointer-level operations.

`Mason.Builder` has API mostly compatible with `Data.ByteString.Builder` but there are some additions to the original API:

* `toStrictByteString` produces a strict `ByteString` directly.
* `hPutBuilderLen` writes a builder to a handle and returns the number of bytes.
* `sendBuilder` sends the content of `Builder` over a socket.
* `withPopper` turns a builder into http-client's[GivesPopper](http://hackage.haskell.org/package/http-client-0.7.2.1/docs/Network-HTTP-Client.html#t:GivesPopper)
* `toStreamingBody` creates wai's [StreamingBody](http://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:StreamingBody)

Usage
----

Replace `Data.ByteString.Builder` with `Mason.Builder`. Note that if you have `Builder` in the type signature, you'll need `RankNTypes` extensions because of the design explained below. Alternatively, you can also import `Mason.Builder.Compat` which has an API almost compatible with `Data.ByteString.Builder`.

Performance
----

As long as the code is optimised, mason's builder can be very fast (twice or more as bytestring). Make sure that functions returning `Builder`s are well inlined.

Serialisation of JSON-like structure:

```
mason/hPutBuilder                        mean 274.7 μs  ( +- 49.40 μs  )
fast-builder/hPutBuilder                 mean 399.9 μs  ( +- 76.05 μs  )
bytestring/hPutBuilder                   mean 335.1 μs  ( +- 86.96 μs  )
mason/toStrictByteString                 mean 106.6 μs  ( +- 6.680 μs  )
fast-builder/toStrictByteString          mean 254.8 μs  ( +- 31.64 μs  )
bytestring/toLazyByteString              mean 283.3 μs  ( +- 24.26 μs  )
mason/toLazyByteString                   mean 127.2 μs  ( +- 25.86 μs  )
fast-builder/toLazyByteString            mean 249.0 μs  ( +- 25.60 μs  )
bytestring/toLazyByteString              mean 263.4 μs  ( +- 9.401 μs  )
```

In the same benchmark application, the allocation footprint of mason is feathery.

```
toStrictByteString
mason           291,112    0
fast-builder    991,016    0
bytestring    1,158,584    0 (toStrict . toLazyByteString)

toLazyByteString
Case          Allocated  GCs
mason           228,936    0
fast-builder    903,752    0
bytestring    1,101,448    0
```

`doubleDec` employs Grisu3 which grants ~20x speedup over `show`-based implementation.

```
mason/double                             mean 116.2 ns  ( +- 6.654 ns  )
fast-builder/double                      mean 2.183 μs  ( +- 85.80 ns  )
bytestring/double                        mean 2.312 μs  ( +- 118.8 ns  )
```

You can find more benchmarks below:

* [bytes-builder-shootout](https://github.com/andrewthad/bytes-builder-shootout)

<details>
<summary>Click to expand</summary>
treeToHex-2000/small-bytearray-builder   mean 44.01 μs  ( +- 1.620 μs  )
treeToHex-2000/fast-builder              mean 34.40 μs  ( +- 390.1 ns  )
treeToHex-2000/bytestring                mean 58.76 μs  ( +- 3.843 μs  )
treeToHex-2000/mason                     mean 41.08 μs  ( +- 180.8 ns  )
treeToHex-9000/small-bytearray-builder   mean 191.8 μs  ( +- 1.835 μs  )
treeToHex-9000/bytestring                mean 284.8 μs  ( +- 1.156 μs  )
treeToHex-9000/mason                     mean 181.2 μs  ( +- 386.3 ns  )
short-text-tree-1000/small-bytearray-builder mean 26.54 μs  ( +- 34.08 ns  )
short-text-tree-1000/fast-builder        mean 37.51 μs  ( +- 99.85 ns  )
short-text-tree-1000/bytestring          mean 37.95 μs  ( +- 167.5 ns  )
short-text-tree-1000/mason               mean 26.87 μs  ( +- 312.4 ns  )
byte-tree-2000/small-bytearray-builder   mean 30.53 μs  ( +- 51.53 ns  )
byte-tree-2000/fast-builder              mean 26.91 μs  ( +- 592.2 ns  )
byte-tree-2000/bytestring                mean 54.40 μs  ( +- 1.743 μs  )
byte-tree-2000/mason                     mean 34.34 μs  ( +- 193.5 ns  )
</details>

* [haskell-perf/strict-bytestring-builder](https://github.com/haskell-perf/strict-bytestring-builders)

<details>
<summary>Click to expand</summary>
averagedAppends-1/byteStringStrictBuilder mean 116.3 ns  ( +- 6.479 ns  )
averagedAppends-1/byteStringTreeBuilder  mean 181.7 ns  ( +- 20.88 ns  )
averagedAppends-1/fastBuilder            mean 181.5 ns  ( +- 7.219 ns  )
averagedAppends-1/bufferBuilder          mean 728.5 ns  ( +- 9.114 ns  )
averagedAppends-1/byteString             mean 358.7 ns  ( +- 4.663 ns  )
averagedAppends-1/blazeBuilder           mean 356.0 ns  ( +- 7.604 ns  )
averagedAppends-1/binary                 mean 635.0 ns  ( +- 7.936 ns  )
averagedAppends-1/cereal                 mean 638.6 ns  ( +- 12.40 ns  )
averagedAppends-1/mason                  mean 155.2 ns  ( +- 2.000 ns  )
averagedAppends-100/byteStringStrictBuilder mean 7.290 μs  ( +- 99.74 ns  )
averagedAppends-100/byteStringTreeBuilder mean 13.40 μs  ( +- 283.4 ns  )
averagedAppends-100/fastBuilder          mean 13.07 μs  ( +- 418.2 ns  )
averagedAppends-100/bufferBuilder        mean 19.57 μs  ( +- 5.644 μs  )
averagedAppends-100/byteString           mean 17.31 μs  ( +- 1.609 μs  )
averagedAppends-100/blazeBuilder         mean 19.15 μs  ( +- 6.533 μs  )
averagedAppends-100/binary               mean 48.26 μs  ( +- 727.1 ns  )
averagedAppends-100/cereal               mean 51.57 μs  ( +- 21.81 μs  )
averagedAppends-100/mason                mean 12.07 μs  ( +- 233.8 ns  )
averagedAppends-10000/byteStringStrictBuilder mean 1.038 ms  ( +- 18.63 μs  )
averagedAppends-10000/byteStringTreeBuilder mean 1.989 ms  ( +- 70.63 μs  )
averagedAppends-10000/fastBuilder        mean 1.611 ms  ( +- 42.24 μs  )
averagedAppends-10000/bufferBuilder      mean 1.895 ms  ( +- 25.09 μs  )
averagedAppends-10000/byteString         mean 2.248 ms  ( +- 40.99 μs  )
averagedAppends-10000/blazeBuilder       mean 2.394 ms  ( +- 1.016 ms  )
averagedAppends-10000/binary             mean 6.503 ms  ( +- 157.6 μs  )
averagedAppends-10000/cereal             mean 6.458 ms  ( +- 221.6 μs  )
averagedAppends-10000/mason              mean 1.738 ms  ( +- 25.89 μs  )
regularConcat-100/byteStringStrictBuilder mean 1.606 μs  ( +- 32.93 ns  )
regularConcat-100/byteStringTreeBuilder  mean 2.000 μs  ( +- 43.73 ns  )
regularConcat-100/fastBuilder            mean 1.364 μs  ( +- 37.95 ns  )
regularConcat-100/bufferBuilder          mean 2.204 μs  ( +- 48.40 ns  )
regularConcat-100/byteString             mean 1.253 μs  ( +- 25.68 ns  )
regularConcat-100/blazeBuilder           mean 1.317 μs  ( +- 24.05 ns  )
regularConcat-100/binary                 mean 2.845 μs  ( +- 62.24 ns  )
regularConcat-100/cereal                 mean 3.021 μs  ( +- 48.53 ns  )
regularConcat-100/mason                  mean 1.405 μs  ( +- 35.11 ns  )
regularConcat-10000/byteStringStrictBuilder mean 321.3 μs  ( +- 11.13 μs  )
regularConcat-10000/byteStringTreeBuilder mean 349.1 μs  ( +- 4.359 μs  )
regularConcat-10000/fastBuilder          mean 121.0 μs  ( +- 1.755 μs  )
regularConcat-10000/bufferBuilder        mean 156.1 μs  ( +- 2.050 μs  )
regularConcat-10000/byteString           mean 106.6 μs  ( +- 1.355 μs  )
regularConcat-10000/blazeBuilder         mean 110.8 μs  ( +- 1.397 μs  )
regularConcat-10000/binary               mean 308.1 μs  ( +- 5.346 μs  )
regularConcat-10000/cereal               mean 352.0 μs  ( +- 6.142 μs  )
regularConcat-10000/mason                mean 130.2 μs  ( +- 10.39 μs  )
</details>

Architecture
----

Mason's builder is a function that takes a purpose-dependent environment and a buffer. There is little intermediate structure involved; almost everything runs in one pass. This design is inspired by [fast-builder](http://hackage.haskell.org/package/fast-builder).

```haskell
type Builder = forall s. Buildable s => BuilderFor s

newtype BuilderFor s = Builder { unBuilder :: s -> Buffer -> IO Buffer }

data Buffer = Buffer
  { bEnd :: {-# UNPACK #-} !(Ptr Word8) -- ^ end of the buffer (next to the last byte)
  , bCur :: {-# UNPACK #-} !(Ptr Word8) -- ^ current position
  }

class Buildable s where
  byteString :: B.ByteString -> BuilderFor s
  flush :: BuilderFor s
  allocate :: Int -> BuilderFor s
```

Instances of the `Buildable` class implement purpose-specific behaviour (e.g. exponentially allocate a buffer, flush to disk). This generic interface also allows creative uses of Builders such as on-the-fly compression.

`Builder` has a smart constructor called `ensure`:

```haskell
ensure :: Int -> (Buffer -> IO Buffer) -> Builder
```

`ensure n f` secures at least `n` bytes in the buffer and passes the pointer to `f`. This gives rise to monoid homorphism; namely, `ensure m f <> ensure n g` will fuse into `ensure (m + n) (f >=> g)` so don't worry about the overhead of bound checking.

Creating your own primitives
----

The easiest way to create a new primitive is `withPtr`, a simplified version of `ensure`. This is quite convenient for calling foreign functions or anything low-level.

```haskell
-- | Construct a 'Builder' from a "poke" function.
withPtr :: Int -- ^ number of bytes to allocate (if needed)
  -> (Ptr Word8 -> IO (Ptr Word8)) -- ^ return a next pointer after writing
  -> Builder

grisu v = withPtr 24 $ \ptr -> do
  n <- dtoa_grisu3 v ptr
  return $ plusPtr ptr (fromIntegral n)

foreign import ccall unsafe "static dtoa_grisu3"
  dtoa_grisu3 :: Double -> Ptr Word8 -> IO CInt
```
