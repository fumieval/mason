{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Mason.Builder.Internal (Builder
  , BuilderFor(..)
  , Buildable(..)
  , GrowingBuffer(..)
  , Buffer(..)
  , byteStringCopy
  , shortByteString
  , toStrictByteString
  , Channel(..)
  , toLazyByteString
  , stringUtf8
  , lengthPrefixedWithin
  , primBounded
  , primFixed
  , primMapListFixed
  , primMapListBounded
  , primMapByteStringFixed
  , primMapLazyByteStringFixed
  , hPutBuilderLen
  , PutBuilderEnv(..)
  , encodeUtf8BuilderEscaped
  , sendBuilder
  , SocketEnv(..)
  -- * Internal
  , Poke(..)
  , poke
  , pokeBoundedPrim
  , pokeFixedPrim
  ) where

import Control.Concurrent
import Control.Exception (throw)
import Control.Monad
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Short.Internal as SB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as B
import Data.Text.Internal.Unsafe.Shift (shiftR)
import Data.Text.Internal.Unsafe.Char (ord)
import System.IO
import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr
import Data.IORef
import Data.Word (Word8)
import Data.String
import qualified Foreign.Storable as S
import System.IO.Unsafe
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as T
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S

data Poke = Poke {-# UNPACK #-} !Int (Ptr Word8 -> IO (Ptr Word8))

instance Semigroup Poke where
  Poke m f <> Poke n g = Poke (m + n) (f >=> g)
  {-# INLINE[1] (<>) #-}

instance Monoid Poke where
  mempty = Poke 0 pure

type Builder = forall s. Buildable s => BuilderFor s

newtype BuilderFor s = Builder { unBuilder :: s -> Buffer -> IO Buffer }

class Buildable s where
  byteString :: B.ByteString -> BuilderFor s
  -- | Flush the content of the internal buffer.
  flush :: BuilderFor s
  -- | Allocate a buffer with at least the given length.
  allocate :: Int -> BuilderFor s

data Buffer = Buffer
  { bEnd :: {-# UNPACK #-} !(Ptr Word8)
  , bCur :: {-# UNPACK #-} !(Ptr Word8)
  }

byteStringCopy :: Buildable s => B.ByteString -> BuilderFor s
byteStringCopy = \(B.PS fsrc ofs len) -> ensure len $ \(Buffer end ptr) -> do
  withForeignPtr fsrc $ \src -> B.memcpy ptr (src `plusPtr` ofs) len
  return $ Buffer end (ptr `plusPtr` len)
{-# INLINE byteStringCopy #-}

poke :: Buildable s => Poke -> BuilderFor s
poke (Poke len run) = ensure len
  $ \(Buffer end dst) -> Buffer end <$> run dst
{-# INLINE[1] poke #-}

{-# RULES "<>/poke" forall a b. poke a <> poke b = poke (a <> b) #-}

shortByteString :: SB.ShortByteString -> Builder
shortByteString = \src -> let len = SB.length src in ensure len $ \(Buffer end ptr) ->
  Buffer end (ptr `plusPtr` len)
  <$ SB.copyToPtr src 0 ptr len
{-# INLINE shortByteString #-}

-- | Ensure that the given number of bytes is available in the buffer.
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

-- | Run a builder within a buffer and prefix by the length.
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
  Builder f <> Builder g = Builder $ \e -> f e >=> g e
  {-# INLINE[1] (<>) #-}

instance Monoid (BuilderFor a) where
  mempty = Builder $ const pure
  {-# INLINE mempty #-}

-- | UTF-8 encode a 'String'.
{-# INLINE stringUtf8 #-}
stringUtf8 :: String -> Builder
stringUtf8 = primMapListBounded P.charUtf8

instance Buildable s => IsString (BuilderFor s) where
  fromString = stringUtf8

pokeBoundedPrim :: B.BoundedPrim a -> a -> Poke
pokeBoundedPrim bp a = Poke (B.sizeBound bp) (B.runB bp a)
{-# INLINE pokeBoundedPrim #-}

pokeFixedPrim :: B.FixedPrim a -> a -> Poke
pokeFixedPrim fp a = Poke (B.size fp) (\ptr -> plusPtr ptr (B.size fp) <$ B.runF fp a ptr)
{-# INLINE pokeFixedPrim #-}

primBounded :: B.BoundedPrim a -> a -> Builder
primBounded bp a = poke $ pokeBoundedPrim bp a
{-# INLINE primBounded #-}

primFixed :: B.FixedPrim a -> a -> Builder
primFixed fp a = poke $ pokeFixedPrim fp a
{-# INLINE primFixed #-}

primMapListFixed :: B.FixedPrim a -> [a] -> Builder
primMapListFixed fp = poke . foldMap (pokeFixedPrim fp)
{-# INLINE primMapListFixed #-}

primMapListBounded :: B.BoundedPrim a -> [a] -> Builder
primMapListBounded bp = poke . foldMap (pokeBoundedPrim bp)
{-# INLINE primMapListBounded #-}

primMapByteStringFixed :: B.FixedPrim Word8 -> B.ByteString -> Builder
primMapByteStringFixed fp = poke . B.foldr (mappend . pokeFixedPrim fp) mempty
{-# INLINE primMapByteStringFixed #-}

primMapLazyByteStringFixed :: B.FixedPrim Word8 -> BL.ByteString -> Builder
primMapLazyByteStringFixed fp = poke . BL.foldr (mappend . pokeFixedPrim fp) mempty
{-# INLINE primMapLazyByteStringFixed #-}

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

toStrictByteString :: BuilderFor GrowingBuffer -> B.ByteString
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
    when (len > 0) $ putMVar v $! B.unsafeCreate len $ \dst -> B.memcpy dst ptr0 len
    return $! Buffer end ptr0
  {-# INLINE flush #-}
  allocate = allocateConstant chBuffer
  {-# INLINE allocate #-}

toLazyByteString :: BuilderFor Channel -> BL.ByteString
toLazyByteString body = unsafePerformIO $ do
  resp <- newEmptyMVar

  let initialSize = 4096
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr

  let final (Left e) = throw e
      final (Right _) = putMVar resp B.empty
  _ <- flip forkFinally final $ unBuilder (body <> flush) (Channel resp ref)
    $ Buffer (ptr `plusPtr` initialSize) ptr

  let go _ = unsafePerformIO $ do
        bs <- takeMVar resp
        return $! if B.null bs
          then BL.empty
          else BL.Chunk bs (go ())
  return $ go ()
{-# INLINE toLazyByteString #-}

data PutBuilderEnv = PBE
  { pbHandle :: !Handle
  , pbBuffer :: !(IORef (ForeignPtr Word8))
  , pbTotal :: !(IORef Int)
  }

-- | Allocate a new buffer.
allocateConstant :: (s -> IORef (ForeignPtr Word8)) -> Int -> BuilderFor s
allocateConstant f len = Builder $ \env (Buffer _ _) -> do
  fptr <- mallocForeignPtrBytes len
  writeIORef (f env) fptr
  let ptr1 = unsafeForeignPtrToPtr fptr
  return $! Buffer (ptr1 `plusPtr` len) ptr1
{-# INLINE allocateConstant #-}

instance Buildable PutBuilderEnv where
  byteString bs
    | len > 4096 = mappend flush $ Builder $ \(PBE h _ _) buf -> do
      B.hPut h bs
      return buf
    | otherwise = byteStringCopy bs
    where
      len = B.length bs
  {-# INLINE byteString #-}

  flush = Builder $ \(PBE h ref counter) (Buffer end ptr) -> do
    ptr0 <- unsafeForeignPtrToPtr <$> readIORef ref
    let len = minusPtr ptr ptr0
    modifyIORef' counter (+len)
    hPutBuf h ptr0 len
    return $! Buffer end ptr0
  {-# INLINE flush #-}

  allocate = allocateConstant pbBuffer
  {-# INLINE allocate #-}

-- | Write a 'Builder' into a handle and obtain the number of bytes written.
-- 'flush' does not imply actual disk operations. Set 'NoBuffering' if you want
-- it to write the content immediately.
hPutBuilderLen :: Handle -> BuilderFor PutBuilderEnv -> IO Int
hPutBuilderLen h b = do
  let initialSize = 4096
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr
  counter <- newIORef 0
  _ <- unBuilder (b <> flush) (PBE h ref counter) (Buffer (ptr `plusPtr` initialSize) ptr)
  readIORef counter
{-# INLINE hPutBuilderLen #-}

data SocketEnv = SE
  { seSocket :: !S.Socket
  , seBuffer :: !(IORef (ForeignPtr Word8))
  , seCounter :: !(IORef Int)
  }

sendBufRange :: S.Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
sendBufRange sock ptr0 ptr1 = go ptr0 where
  go p
    | p >= ptr1 = return ()
    | otherwise = do
      sent <- S.sendBuf sock p (minusPtr ptr1 p)
      S.withFdSocket sock $ threadWaitWrite . fromIntegral
      when (sent > 0) $ go $ p `plusPtr` sent

instance Buildable SocketEnv where
  byteString bs
    | len > 4096 = mappend flush $ Builder $ \(SE sock _ _) (Buffer end ptr) -> do
      S.sendAll sock bs
      return $! Buffer end ptr
    | otherwise = byteStringCopy bs
    where
      len = B.length bs
  {-# INLINE byteString #-}

  flush = Builder $ \(SE sock ref counter) (Buffer end ptr1) -> do
    ptr0 <- unsafeForeignPtrToPtr <$> readIORef ref
    sendBufRange sock ptr0 ptr1
    modifyIORef' counter (+minusPtr ptr1 ptr0)
    return $! Buffer end ptr0
  {-# INLINE flush #-}

  allocate = allocateConstant seBuffer

-- | Write a 'Builder' into a handle and obtain the number of bytes written.
sendBuilder :: S.Socket -> BuilderFor SocketEnv -> IO Int
sendBuilder sock b = do
  let initialSize = 4096
  fptr <- mallocForeignPtrBytes initialSize
  ref <- newIORef fptr
  let ptr = unsafeForeignPtrToPtr fptr
  counter <- newIORef 0
  _ <- unBuilder (b <> flush) (SE sock ref counter) (Buffer (ptr `plusPtr` initialSize) ptr)
  readIORef counter
{-# INLINE sendBuilder #-}

{-# INLINE encodeUtf8BuilderEscaped #-}
encodeUtf8BuilderEscaped :: B.BoundedPrim Word8 -> T.Text -> Builder
encodeUtf8BuilderEscaped be = mkBuildstep where
  bound = max 4 $ B.sizeBound be

  mkBuildstep (T.Text arr off len) = Builder $ outerLoop off where
    iend = off + len

    outerLoop !i0 env !br@(Buffer ope op0)
      | i0 >= iend       = return br
      | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
      -- TODO: Use a loop with an integrated bound's check if outRemaining
      -- is smaller than 8, as this will save on divisions.
      | otherwise        = unBuilder (ensure bound (outerLoop i0 env)) env br
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
              | otherwise =
                  outerLoop i env (Buffer ope op)
              where
                poke8 :: Integral a => Int -> a -> IO ()
                poke8 j v = S.poke (op `plusPtr` j) (fromIntegral v :: Word8)

foreign import ccall unsafe "memmove"
    c_memmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
