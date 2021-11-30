import qualified Fast as F
import qualified Bstr as B
import qualified Mason as M
import Control.Concurrent
import Control.DeepSeq
import Test.Tasty.Bench
import Data.Aeson (Value, decode', encode)
import qualified Data.ByteString.Lazy as L
import System.IO

readTestData :: IO Value
readTestData = do
  Just val <- decode' <$> L.readFile "aeson/twitter100-mangled.json"
  pure val

main :: IO ()
main = runInUnboundThread $ withFile "bench.json" WriteMode $ \h -> do
  hSetBuffering h NoBuffering

  json <- readTestData

  rnf json `seq` return ()

  defaultMain
    [ bench "mason/double" $ nf (\x -> M.toStrictByteString $ M.doubleDec x) (pi * 1e6)
    , bench "fast-builder/double" $ nf (F.toStrictByteString . F.doubleDec) (pi * 1e6)
    , bench "bytestring/double" $ nf (B.toStrictByteString . B.doubleDec) (pi * 1e6)
    , bench "mason/literal" $ nf M.toStrictByteString M.literal
    , bench "fast-builder/literal" $ nf F.toStrictByteString F.literal
    , bench "bytestring/literal" $ nf B.toStrictByteString B.literal
    , bench "mason/hPutBuilder" $ nfIO (M.putValue h json)
    , bench "fast-builder/hPutBuilder" $ nfIO (F.putValue h json)
    , bench "bytestring/hPutBuilder" $ nfIO (B.putValue h json)
    , bench "mason/toStrictByteString" $ nf M.valueToByteString json
    , bench "fast-builder/toStrictByteString" $ nf F.valueToByteString json
    , bench "bytestring/toLazyByteString" $ nf B.valueToByteString json
    , bench "mason/toLazyByteString" $ nf M.valueToLazyByteString json
    , bench "fast-builder/toLazyByteString" $ nf F.valueToLazyByteString json
    , bench "bytestring/toLazyByteString" $ nf B.valueToLazyByteString json
    , bench "aeson" $ nf encode json
    ]
