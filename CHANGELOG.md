## 0.2.5

* Supported GHC 9.2.1

## 0.2.4

* Generalised the argument `intersperse`, `unlines`, `unwords` from a list to any `Foldable`
* Supported GHC 9.0.1

## 0.2.3

* Added `intDecPadded`
* Exported backend types: `StrictByteStringBackend`, `LazyByteStringBackend` and `BufferedIOBackend`

## 0.2.2

* Added `withPopper` and `toStreamingBody`
* Added `viaShow`
* Added `intersperse`, `unwords` and `unlines`
* Optmised the internal representation

## 0.2.1

* Added `Mason.Builder.Compat`

## 0.2

* Added `doubleFixed`, `doubleSI` and `doubleExp`
* Added `textUtf8`
* Added `prefixVarInt`, `wordVLQ` and `intVLQ`
* Renamed `padded` and `zeroPadded` to `paddedBoundedPrim` and `zeroPaddedBoundedPrim` respectively
* Added `Mason.Builder.Dynamic`

## 0 -- 2019-12-05

* First version. Released on an unsuspecting world.
