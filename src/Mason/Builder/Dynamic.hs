module Mason.Builder.Dynamic
  ( DynBuilder
  , DynamicBackend(..)
  -- * Runners
  , toStrictByteString
  , toLazyByteString
  , hPutBuilderLen
  , hPutBuilder
  , sendBuilder
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Mason.Builder as B
import qualified Mason.Builder.Internal as B
import Network.Socket (Socket)
import System.IO (Handle)

data DynamicBackend = DynGrowingBuffer !B.GrowingBuffer
  | DynChannel !B.DCEnv
  | DynPutEnv !B.PutEnv

instance B.Buildable DynamicBackend where
  byteString bs = B.Builder $ \env buf -> case env of
    DynGrowingBuffer e -> B.unBuilder (B.byteString bs) e buf
    DynChannel e -> B.unBuilder (B.byteString bs) e buf
    DynPutEnv e -> B.unBuilder (B.byteString bs) e buf
  flush = B.Builder $ \env buf -> case env of
    DynGrowingBuffer e -> B.unBuilder B.flush e buf
    DynChannel e -> B.unBuilder B.flush e buf
    DynPutEnv e -> B.unBuilder B.flush e buf
  allocate n = B.Builder $ \env buf -> case env of
    DynGrowingBuffer e -> B.unBuilder (B.allocate n) e buf
    DynChannel e -> B.unBuilder (B.allocate n) e buf
    DynPutEnv e -> B.unBuilder (B.allocate n) e buf

-- | Builder with a fixed set of backends. This helps reducing code size
-- and unoptimised code especially on complex/recursive structures, at the cost of
-- extensibility.
type DynBuilder = B.BuilderFor DynamicBackend

toStrictByteString :: DynBuilder -> B.ByteString
toStrictByteString b = B.toStrictByteString $ B.Builder $ B.unBuilder b . DynGrowingBuffer
{-# INLINE toStrictByteString #-}

toLazyByteString :: DynBuilder -> BL.ByteString
toLazyByteString b = B.toLazyByteString $ B.Builder $ B.unBuilder b . DynChannel
{-# INLINE toLazyByteString #-}

hPutBuilder :: Handle -> DynBuilder -> IO ()
hPutBuilder h b = B.hPutBuilder h $ B.Builder $ B.unBuilder b . DynPutEnv
{-# INLINE hPutBuilder #-}

hPutBuilderLen :: Handle -> DynBuilder -> IO Int
hPutBuilderLen h b = B.hPutBuilderLen h $ B.Builder $ B.unBuilder b . DynPutEnv
{-# INLINE hPutBuilderLen #-}

sendBuilder :: Socket -> DynBuilder -> IO Int
sendBuilder h b = B.sendBuilder h $ B.Builder $ B.unBuilder b . DynPutEnv
{-# INLINE sendBuilder #-}
