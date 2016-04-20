module Utils
  where

import qualified Data.ByteString.Builder as BB (toLazyByteString, word8)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.ByteString as BS (ByteString)

import DataSetParser (parseDataSet, IPDataSet)

loadFactorExperiment :: String -> (IPDataSet -> Int) -> FilePath -> IO ()
loadFactorExperiment hash eval dataset = do
    addrs <- parseDataSet dataset
    let slots = eval addrs
    putStrLn $ hash ++ " | " ++ dataset ++ " : " ++ show slots ++ " slots."

ipToByteString :: [Int] -> BS.ByteString
ipToByteString = BL.toStrict . BB.toLazyByteString . mconcat . map (BB.word8 . fromIntegral)
