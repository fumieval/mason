mason: alacritous builder library
====

mason is a builder & IO library.

* __Fast__: much faster than bytestring's Builder.
* __Extensible__: Builders can be consumed in a user-defined way.
* __Hackable__: Low-level APIs are exposed. It's easy to plug in even pointer-level operations.

`Mason.Builder` has API mostly compatible with `Data.ByteString.Builder` but there are some additions to the original API:

* `toStrictByteString` produces a strict `ByteString` directly.
* `hPutBuilderLen` writes a builder to a handle and returns the number of bytes.
* `sendBuilder` sends the content of `Builder` over a socket.

Usage
----

Replace `Data.ByteString.Builder` with `Mason.Builder`. Note that if you have `Builder` in the type signature, you'll need `RankNTypes` extensions because of the design explained below.

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

TBD: more benchmarks

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
