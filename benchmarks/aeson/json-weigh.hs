module Main where
import qualified Fast as F
import qualified Bstr as B
import qualified Mason as M
import Control.Concurrent
import Control.DeepSeq
import Data.Aeson (decode', encode)
import qualified Data.ByteString.Lazy as L
import System.IO
import Weigh

main :: IO ()
main = do
  Just json <- decode' <$> L.readFile "aeson/twitter100-mangled.json"
  rnf json `seq` return ()

  mainWith $ do
    func "mason" M.valueToByteString json
    func "fast-builder" F.valueToByteString json
    func "bytestring" B.valueToByteString json
