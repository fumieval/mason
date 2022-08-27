{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Mason.Builder.Internal (Builder
  , BuilderFor(..)
  , BState
  , Buildable(..)
  , GrowingBuffer(..)
  , Buffer(..)
  , pattern Builder
  , unBuilder
  , byteStringCopy
  , shortByteString
  , StrictByteStringBackend
  , toStrictByteString
  , Channel(..)
  , LazyByteStringBackend
  , toLazyByteString
  , withPopper
  , StreamingBackend(..)
  , toStreamingBody
  , stringUtf8
  , lengthPrefixedWithin
  , primBounded
  , primFixed
  , primMapListFixed
  , primMapListBounded
  , primMapByteStringFixed
  , primMapLazyByteStringFixed
  , PutEnv(..)
  , BufferedIOBackend
  , hPutBuilderLen
  , encodeUtf8BuilderEscaped
  , sendBuilder
  , cstring
  , cstringUtf8
  , withPtr
  , storable
  , paddedBoundedPrim
  , zeroPaddedBoundedPrim
  -- * Internal
  , ensure
  , allocateConstant
  , withGrisu3
  , withGrisu3Rounded
  , roundDigit
  ) where

import Control.Concurrent
import Control.Exception (throw)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Short.Internal as SB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as B
import System.IO
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr
import Foreign.Marshal.Array (allocaArray)
import Data.IORef
import Data.Word (Word8)
import Data.String
import Foreign.Storable as S
import System.IO.Unsafe
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as T
import qualified Network.Socket as S
import GHC.Prim (plusAddr#, indexWord8OffAddr#, RealWorld, Addr#, State# )
import GHC.Ptr (Ptr(..))
import GHC.Word (Word8(..))
import GHC.Base (unpackCString#, unpackCStringUtf8#, unpackFoldrCString#, build, IO(..), unIO)

#if MIN_VERSION_text(2,0,0)
#else
-- imports required by 'encodeUtf8BuilderEscaped'
import Data.Bits ((.&.), shiftR)
import Data.Text.Internal.Unsafe.Char (ord)
import qualified Data.Text.Internal.Encoding.Utf16 as U16
#endif

-- https://www.haskell.org/ghc/blog/20210607-the-keepAlive-story.html
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr fo f = do
  r <- f (unsafeForeignPtrToPtr fo)
  touchForeignPtr fo
  return r

-- | The Builder type. Requires RankNTypes extension
type Builder = forall s. Buildable s => BuilderFor s

-- | Builder specialised for a backend
newtype BuilderFor s = RawBuilder { unRawBuilder :: s -> BState -> BState }

unBuilder :: BuilderFor s -> s -> Buffer -> IO Buffer
unBuilder (RawBuilder f) = \env (Buffer (Ptr ptr) (Ptr end)) -> IO (\s -> case f env (# ptr, end, s #) of
   (# ptr', end', s' #) -> (# s', Buffer (Ptr ptr') (Ptr end') #))
{-# INLINE unBuilder #-}

pattern Builder :: (s -> Buffer -> IO Buffer) -> BuilderFor s
pattern Builder f <- (unBuilder -> f) where
  Builder f = RawBuilder $ \env (# ptr, end, s #) -> case unIO (f env (Buffer (Ptr ptr) (Ptr end))) s of
    (# s', Buffer (Ptr ptr') (Ptr end') #) -> (# ptr', end', s' #)

{-# COMPLETE Builder #-}

type BState = (#Addr#, Addr#, State# RealWorld #)

-- | This class is used to provide backend-specific operations for running a 'Builder'.
class Buildable s where
  -- | Put a 'B.ByteString'.
  byteString :: B.ByteString -> BuilderFor s
  byteString = byteStringCopy
  {-# INLINE byteString #-}
  -- | Flush the content of the internal buffer.
  flush :: BuilderFor s
  -- | Allocate a buffer with at least the given length.
  allocate :: Int -> BuilderFor s

-- | Buffer pointers
data Buffer = Buffer
  { bEnd :: {-# UNPACK #-} !(Ptr Word8) -- ^ end of the buffer (next to the last byte)
  , bCur :: {-# UNPACK #-} !(Ptr Word8) -- ^ current position
  }

-- | Copy a 'B.ByteString' to a buffer.
byteStringCopy :: Buildable s => B.ByteString -> BuilderFor s
byteStringCopy = \(B.PS fsrc ofs len) -> withPtr len $ \ptr -> do
  unsafeWithForeignPtr fsrc $ \src -> B.memcpy ptr (src `plusPtr` ofs) len
  return $ ptr `plusPtr` len
{-# INLINE byteStringCopy #-}

-- | Copy a 'SB.ShortByteString' to a buffer.
shortByteString :: SB.ShortByteString -> Builder
shortByteString = \src -> let len = SB.length src in withPtr len $ \ptr ->
  plusPtr ptr len <$ SB.copyToPtr src 0 ptr len
{-# INLINE shortByteString #-}

-- | Construct a 'Builder' from a "poke" function.
withPtr :: Buildable s
  => Int -- ^ number of bytes to allocate (if needed)
  -> (Ptr Word8 -> IO (Ptr Word8)) -- ^ return a next pointer after writing
  -> BuilderFor s
withPtr n f = ensure n $ \(Buffer e p) -> Buffer e <$> f p
{-# INLINE withPtr #-}

-- | Turn a 'Storable' value into a 'Builder'
storable :: Storable a => a -> Builder
storable a = withPtr (sizeOf a) $ \p -> plusPtr p (sizeOf a) <$ poke (castPtr p) a
{-# INLINE storable #-}

-- | Ensure that the given number of bytes is available in the buffer. Subject to semigroup fusion
ensure :: Int -> (Buffer -> IO Buffer) -> Builder
ensure mlen cont = Builder $ \env buf@(Buffer end ptr) ->
  if ptr `plusPtr` mlen >= end
    then do
      buf'@(Buffer end' ptr') <- unBuilder flush env buf
      if mlen <= minusPtr end' ptr'
        then cont buf'
        else unBuilder (allocate mlen) env buf' >>= cont
    else cont buf
{-# INLINE[1] ensure #-}

{-# RULES "<>/ensure" forall m n f g. ensure m f <> ensure n g = ensure (m + n) (f >=> g) #-}

-- | Run a builder within a buffer and prefix it by the length.
lengthPrefixedWithin :: Int -- ^ maximum length
  -> B.BoundedPrim Int -- ^ prefix encoder
  -> BuilderFor () -> Builder
lengthPrefixedWithin maxLen bp builder = ensure (B.sizeBound bp + maxLen) $ \(Buffer end origin) -> do
  let base = origin `plusPtr` B.sizeBound bp
  Buffer _ base' <- unBuilder builder () (Buffer end base)
  let len = minusPtr base' base
  newBase <- B.runB bp len origin
  c_memmove newBase base len
  return $ Buffer end (newBase `plusPtr` len)
{-# INLINE lengthPrefixedWithin #-}

-- | Work with a constant buffer. 'allocate' will always fail.
instance Buildable () where
  byteString = byteStringCopy
  {-# INLINE byteString #-}
  flush = mempty
  {-# INLINE flush #-}
  allocate _ = Builder $ \_ _ -> fail "Mason.Builder.Internal.allocate: can't allocate"
  {-# INLINE allocate #-}

instance Semigroup (BuilderFor s) where
  RawBuilder f <> RawBuilder g = RawBuilder $ \e s -> g e (f e s)
  {-# INLINE[1] (<>) #-}

instance Monoid (BuilderFor a) where
  mempty = RawBuilder (\_ s -> s)
  {-# INLINE mempty #-}

-- | UTF-8 encode a 'String'.
stringUtf8 :: Buildable s => String -> BuilderFor s
stringUtf8 = primMapListBounded P.charUtf8
{-# INLINE [1] stringUtf8 #-}

{-# RULES
"stringUtf8/unpackCStringUtf8#" forall s.
  stringUtf8 (unpackCStringUtf8# s) = cstringUtf8 (Ptr s)

"stringUtf8/unpackCString#" forall s.
  stringUtf8 (unpackCString# s) = cstring (Ptr s)

"stringUtf8/unpackFoldrCString#" forall s.
  stringUtf8 (build (unpackFoldrCString# s)) = cstring (Ptr s)
 #-}

cstring :: Ptr Word8 -> Builder
cstring (Ptr addr0) = Builder $ step addr0
  where
    step addr env br@(Buffer end ptr)
      | W8# ch == 0 = pure br
      | ptr == end = unBuilder (ensure 3 $ step addr env) env br
      | otherwise = do
          S.poke ptr (W8# ch)
          let br' = Buffer end (ptr `plusPtr` 1)
          step (addr `plusAddr#` 1#) env br'
      where
        !ch = indexWord8OffAddr# addr 0#
{-# INLINE cstring #-}

cstringUtf8 :: Ptr Word8 -> Builder
cstringUtf8 (Ptr addr0) = Builder $ step addr0
  where
    step addr env br@(Buffer end ptr)
      | W8# ch == 0 = pure br
      | ptr == end = unBuilder (ensure 3 $ step addr env) env br
        -- NULL is encoded as 0xc0 0x80
      | W8# ch == 0xc0
      , W8# (indexWord8OffAddr# addr 1#) == 0x80 = do
        S.poke ptr 0
        step (addr `plusAddr#` 2#) env (Buffer end (ptr `plusPtr` 1))
      | otherwise = do
        S.poke ptr (W8# ch)
        step (addr `plusAddr#` 1#) env (Buffer end (ptr `plusPtr` 1))
      where
        !ch = indexWord8OffAddr# addr 0#
{-# INLINE cstringUtf8 #-}

instance Buildable s => IsString (BuilderFor s) where
  fromString = stringUtf8
  {-# INLINE fromString #-}

-- | Use 'B.BoundedPrim'
primBounded :: Buildable s => B.BoundedPrim a -> a -> BuilderFor s
primBounded bp = withPtr (B.sizeBound bp) . B.runB bp
{-# INLINE primBounded #-}

-- | Use 'B.FixedPrim'
primFixed :: Buildable s => B.FixedPrim a -> a -> BuilderFor s
primFixed fp a = withPtr (B.size fp) $ \ptr -> (ptr `plusPtr` B.size fp) <$ B.runF fp a ptr
{-# INLINE primFixed #-}

primMapListFixed :: (Foldable t, Buildable s) => B.FixedPrim a -> t a -> BuilderFor s
primMapListFixed fp = foldMap (primFixed fp)
{-# INLINE primMapListFixed #-}

primMapListBounded :: Buildable s => B.BoundedPrim a -> [a] -> BuilderFor s
primMapListBounded bp = foldMap (primBounded bp)
{-# INLINE primMapListBounded #-}

primMapByteStringFixed :: Buildable s => B.FixedPrim Word8 -> B.ByteString -> BuilderFor s
primMapByteStringFixed fp = B.foldr (mappend . primFixed fp) mempty
{-# INLINE primMapByteStringFixed #-}

primMapLazyByteStringFixed :: Buildable s => B.FixedPrim Word8 -> BL.ByteString -> BuilderFor s
primMapLazyByteStringFixed fp = BL.foldr (mappend . primFixed fp) mempty
{-# INLINE primMapLazyByteStringFixed #-}

paddedBoundedPrim
  :: Word8 -- ^ filler
  -> Int -- ^ pad if shorter than this
  -> B.BoundedPrim a
  -> a
  -> Builder
paddedBoundedPrim ch size bp a = ensure (B.sizeBound bp) $ \(Buffer end ptr) -> do
  ptr' <- B.runB bp a ptr
  let len = ptr' `minusPtr` ptr
  let pad = size - len
  when (pad > 0) $ do
    c_memmove (ptr `plusPtr` pad) ptr len
    void $ B.memset ptr ch (fromIntegral pad)
  return $ Buffer end $ ptr' `plusPtr` max pad 0

zeroPaddedBoundedPrim :: Int -> B.BoundedPrim a -> a -> Builder
zeroPaddedBoundedPrim = paddedBoundedPrim 48

newtype GrowingBuffer = GrowingBuffer (IORef (ForeignPtr Word8))

instance Buildable GrowingBuffer where
  byteString = byteStringCopy
  {-# INLINE byteString #-}
  flush = mempty
  {-# INLINE flush #-}
  allocate len = Builder $ \(GrowingBuffer bufferRef) (Buffer _ dst) -> do
    fptr0 <- readIORef bufferRef
    let ptr0 = unsafeForeignPtrToPtr fptr0
    let !pos = dst `minusPtr` ptr0
    let !size' = pos + max len pos
    fptr <- mallocForeignPtrBytes size'
    let !dst' = unsafeForeignPtrToPtr fptr
    B.memcpy dst' ptr0 pos
    writeIORef bufferRef fptr
    return $ Buffer (dst' `plusPtr` size') (dst' `plusPtr` pos)
  {-# INLINE allocate #-}

type StrictByteStringBackend = GrowingBuffer

-- | Create a strict 'B.ByteString'
toStrictByteString :: BuilderFor StrictByteStringBackend -> B.ByteString
toStrictByteString b = unsafePerformIO $ do
  fptr0 <- mallocForeignPtrBytes initialSize
  bufferRef <- newIORef fptr0
  let ptr0 = unsafeForeignPtrToPtr fptr0

  Buffer _ pos <- unBuilder b (GrowingBuffer bufferRef)
    $ Buffer (ptr0 `plusPtr` initialSize) ptr0

  fptr <- readIORef bufferRef
  pure $ B.PS fptr 0 (pos `minusPtr` unsafeForeignPtrToPtr fptr)

  where
    initialSize = 128
{-# INLINE toStrictByteString #-}

data Channel = Channel
  { chResp :: !(MVar B.ByteString)
  , chBuffer :: !(IORef (ForeignPtr Word8))
  }

instance Buildable Channel where
  byteString bs
    | B.length bs < 4096 = byteStringCopy bs
    | otherwise = flush <> Builder (\(Channel v _) b -> b <$ putMVar v bs)
  {-# INLINE byteString #-}
  flush = Builder $ \(Channel v ref) (Buffer end ptr) -> do
    ptr0 <- unsafeForeignPtrToPtr <$> readIORef ref
    let len = minusPtr ptr ptr0
    when (len > 0) $ do
      bs <- B.mallocByteString len
      unsafeWithForeignPtr bs $ \dst -> B.memcpy dst ptr0 len
      putMVar v $ B.PS bs 0 len
    return $! Buffer end ptr0
  {-# INLINE flush #-}
  allocate = allocateConstant chBuffer
  {-# INLINE allocate #-}

type LazyByteStringBackend = Channel

-- | Create a lazy 'BL.ByteString'. Threaded runtime is required.
toLazyByteString :: BuilderFor LazyByteStringBackend -> BL.ByteString
toLazyByteString body = unsafePerformIO $ withPopper body $ \pop -> do
  let go _ = do
        bs <- pop
        return $! if B.null bs
          then BL.empty
          else BL.Chunk bs $ unsafePerformIO $ go ()
  return $ unsafePerformIO $ go ()
{-# INLINE toLazyByteString #-}

-- | Use 'Builder' as a <http://hackage.haskell.org/package/http-client-0.7.1/docs/Network-HTTP-Client.html#t:GivesPopper GivesPopper'
withPopper :: BuilderFor LazyByteStringBackend -> (IO B.ByteString -> IO a) -> IO a
withPopper body cont = do
  resp <- newEmptyMVar

  let initialSize = 4080
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr

  let final (Left e) = putMVar resp (throw e)
      final (Right _) = putMVar resp B.empty
  _ <- flip forkFinally final $ unBuilder (body <> flush) (Channel resp ref)
    $ Buffer (ptr `plusPtr` initialSize) ptr

  cont $ takeMVar resp
{-# INLINE withPopper #-}

-- | Environment for handle output
data PutEnv = PutEnv
  { peThreshold :: !Int
  , pePut :: !(Ptr Word8 -> Ptr Word8 -> IO ())
  -- ^ takes a pointer range and returns the number of bytes written
  , peBuffer :: !(IORef (ForeignPtr Word8))
  , peTotal :: !(IORef Int)
  }

-- | Allocate a new buffer.
allocateConstant :: (s -> IORef (ForeignPtr Word8)) -> Int -> BuilderFor s
allocateConstant f len = Builder $ \env (Buffer _ _) -> do
  fptr <- mallocForeignPtrBytes len
  writeIORef (f env) fptr
  let ptr1 = unsafeForeignPtrToPtr fptr
  return $! Buffer (ptr1 `plusPtr` len) ptr1
{-# INLINE allocateConstant #-}

instance Buildable PutEnv where
  byteString bs@(B.PS fptr ofs len) = Builder $ \env@PutEnv{..} buf -> if len > peThreshold
    then do
      buf' <- unBuilder flush env buf
      unsafeWithForeignPtr fptr $ \ptr -> do
        let ptr0 = ptr `plusPtr` ofs
        pePut ptr0 (ptr0 `plusPtr` len)
      pure buf'
    else unBuilder (byteStringCopy bs) env buf
  {-# INLINE byteString #-}

  flush = Builder $ \PutEnv{..} (Buffer end ptr) -> do
    ptr0 <- unsafeForeignPtrToPtr <$> readIORef peBuffer
    let len = minusPtr ptr ptr0
    modifyIORef' peTotal (+len)
    pePut ptr0 ptr
    return $! Buffer end ptr0
  {-# INLINE flush #-}

  allocate = allocateConstant peBuffer
  {-# INLINE allocate #-}

type BufferedIOBackend = PutEnv

-- | Write a 'Builder' into a handle and obtain the number of bytes written.
-- 'flush' does not imply actual disk operations. Set 'NoBuffering' if you want
-- it to write the content immediately.
hPutBuilderLen :: Handle -> BuilderFor BufferedIOBackend -> IO Int
hPutBuilderLen h b = do
  let initialSize = 4096
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr
  counter <- newIORef 0
  _ <- unBuilder (b <> flush)
    (PutEnv initialSize (\ptr0 ptr1 -> hPutBuf h ptr (minusPtr ptr1 ptr0)) ref counter)
    (Buffer (ptr `plusPtr` initialSize) ptr)
  readIORef counter
{-# INLINE hPutBuilderLen #-}

sendBufRange :: S.Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
sendBufRange sock ptr0 ptr1 = go ptr0 where
  go p
    | p >= ptr1 = return ()
    | otherwise = do
      sent <- S.sendBuf sock p (minusPtr ptr1 p)
      S.withFdSocket sock $ threadWaitWrite . fromIntegral
      when (sent > 0) $ go $ p `plusPtr` sent

-- | Write a 'Builder' into a handle and obtain the number of bytes written.
sendBuilder :: S.Socket -> BuilderFor BufferedIOBackend -> IO Int
sendBuilder sock b = do
  let initialSize = 4096
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr
  counter <- newIORef 0
  _ <- unBuilder (b <> flush)
    (PutEnv initialSize (sendBufRange sock) ref counter)
    (Buffer (ptr `plusPtr` initialSize) ptr)
  readIORef counter
{-# INLINE sendBuilder #-}

{-# INLINE encodeUtf8BuilderEscaped #-}

-- | Encode 'T.Text' with a custom escaping function.
--
-- Note that implementation differs between @text-1.x@ and @text-2.x@ due to the
-- package moving from using UTF-16 to UTF-8 for the internal representation.
encodeUtf8BuilderEscaped :: Buildable s => B.BoundedPrim Word8 -> T.Text -> BuilderFor s

#if MIN_VERSION_text(2,0,0)
encodeUtf8BuilderEscaped be = step where
  bound = max 4 $ B.sizeBound be

  step (T.Text arr off len) = Builder $ loop off where
    iend = off + len
    loop !i0 env !br@(Buffer ope op0)
      | i0 >= iend       = return br
      | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
      | otherwise        = unBuilder (ensure bound (loop i0 env)) env br
      where
        outRemaining = (ope `minusPtr` op0) `quot` bound
        inpRemaining = iend - i0

        goPartial !iendTmp = go i0 op0
          where
            go !i !op
              | i < iendTmp = do
                let w = A.unsafeIndex arr i
                if w < 0x80
                  then B.runB be w op >>= go (i + 1)
                  else poke op w >> go (i + 1) (op `plusPtr` 1)
              | otherwise = loop i env (Buffer ope op)

#else
encodeUtf8BuilderEscaped be = step where
  bound = max 4 $ B.sizeBound be

  step (T.Text arr off len) = Builder $ loop off where
    iend = off + len
    loop !i0 env !br@(Buffer ope op0)
      | i0 >= iend       = return br
      | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
      | otherwise        = unBuilder (ensure bound (loop i0 env)) env br
      where
        outRemaining = (ope `minusPtr` op0) `div` bound
        inpRemaining = iend - i0

        goPartial !iendTmp = go i0 op0
          where
            go !i !op
              | i < iendTmp = case A.unsafeIndex arr i of
                  w | w <= 0x7F -> do
                        B.runB be (fromIntegral w) op >>= go (i + 1)
                    | w <= 0x7FF -> do
                        poke8 0 $ (w `shiftR` 6) + 0xC0
                        poke8 1 $ (w .&. 0x3f) + 0x80
                        go (i + 1) (op `plusPtr` 2)
                    | 0xD800 <= w && w <= 0xDBFF -> do
                        let c = ord $ U16.chr2 w (A.unsafeIndex arr (i+1))
                        poke8 0 $ (c `shiftR` 18) + 0xF0
                        poke8 1 $ ((c `shiftR` 12) .&. 0x3F) + 0x80
                        poke8 2 $ ((c `shiftR` 6) .&. 0x3F) + 0x80
                        poke8 3 $ (c .&. 0x3F) + 0x80
                        go (i + 2) (op `plusPtr` 4)
                    | otherwise -> do
                        poke8 0 $ (w `shiftR` 12) + 0xE0
                        poke8 1 $ ((w `shiftR` 6) .&. 0x3F) + 0x80
                        poke8 2 $ (w .&. 0x3F) + 0x80
                        go (i + 1) (op `plusPtr` 3)
              | otherwise = loop i env (Buffer ope op)
              where
                poke8 :: Integral a => Int -> a -> IO ()
                poke8 j v = S.poke (op `plusPtr` j) (fromIntegral v :: Word8)
#endif

foreign import ccall unsafe "memmove"
    c_memmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

-- | Decimal encoding of a positive 'Double'.
{-# INLINE withGrisu3 #-}
withGrisu3 :: Double -> IO r -> (Ptr Word8 -> Int -> Int -> IO r) -> IO r
withGrisu3 d contFail cont = allocaArray 2 $ \plen -> allocaArray 19 $ \ptr -> do
  let pexp = plusPtr plen (S.sizeOf (undefined :: CInt))
  success <- c_grisu3 (realToFrac d) (ptr `plusPtr` 1) plen pexp
  if success == 0
    then contFail
    else do
      len <- fromIntegral <$> S.peek plen
      e <- fromIntegral <$> S.peek pexp
      cont ptr len (len + e)

{-# INLINE withGrisu3Rounded #-}
withGrisu3Rounded :: Int -> Double -> (Ptr Word8 -> Int -> Int -> IO r) -> IO r
withGrisu3Rounded prec val cont = withGrisu3 val (error "withGrisu3Rounded: failed") $ \ptr len e -> do
  let len' = min prec len
  bump <- roundDigit prec len ptr
  if bump then cont ptr len' (e + 1) else cont (ptr `plusPtr` 1) len' e

-- | Round up to the supplied precision inplace.
roundDigit
  :: Int -- ^ precision
  -> Int -- ^ available digits
  -> Ptr Word8 -- ^ content
  -> IO Bool
roundDigit prec len _ | prec >= len = pure False
roundDigit prec _ ptr = do
  rd <- peekElemOff ptr (prec + 1)
  let carry 0 = do
        poke ptr 49
        pure True
      carry i = do
        d <- peekElemOff ptr i
        if d == 57
          then pokeElemOff ptr i 48 *> carry (i - 1)
          else do
            pokeElemOff ptr i (d + 1)
            pure False
  if rd >= 53
    then carry prec
    else pure False

foreign import ccall unsafe "static grisu3"
  c_grisu3 :: CDouble -> Ptr Word8 -> Ptr CInt -> Ptr CInt -> IO CInt

data StreamingBackend = StreamingBackend
  { sePush :: !(B.ByteString -> IO ())
  , seBuffer :: !(IORef (ForeignPtr Word8))
  }

instance Buildable StreamingBackend where
  byteString bs
    | B.length bs < 4096 = byteStringCopy bs
    | otherwise = flush <> Builder (\env b -> b <$ sePush env bs)
  {-# INLINE byteString #-}
  flush = Builder $ \(StreamingBackend push ref) (Buffer end ptr) -> do
    ptr0 <- unsafeForeignPtrToPtr <$> readIORef ref
    let len = minusPtr ptr ptr0
    when (len > 0) $ push $! B.unsafeCreate len $ \dst -> B.memcpy dst ptr0 len
    return $! Buffer end ptr0
  {-# INLINE flush #-}
  allocate = allocateConstant seBuffer
  {-# INLINE allocate #-}

-- | Convert a 'Builder' into a <http://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html#t:StreamingBody StreamingBody>.
toStreamingBody :: BuilderFor StreamingBackend -> (BB.Builder -> IO ()) -> IO () -> IO ()
toStreamingBody body = \write _ -> do
  let initialSize = 4080
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr
  Buffer _ ptr2 <- unBuilder body
    (StreamingBackend (write . BB.byteString) ref)
    (Buffer (ptr `plusPtr` initialSize) ptr)
  fptr' <- readIORef ref
  let ptr1 = unsafeForeignPtrToPtr fptr'
  write $ BB.byteString $ B.PS fptr' 0 (minusPtr ptr2 ptr1)
