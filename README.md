mason: alacritous builder library
====

mason is a very performant, extensible builder & IO library.

The design is inspired by [fast-builder](http://hackage.haskell.org/package/fast-builder) and
`Mason.Builder` has API mostly compatible with `Data.ByteString.Builder`.

There are some additions to the original API:

* `toStrictByteString` produces a strict `ByteString` directly.
* `hPutBuilderLen` writes a builder to a handle and returns the number of bytes.
* `sendBuilder` sends the content of `Builder` over a socket.
