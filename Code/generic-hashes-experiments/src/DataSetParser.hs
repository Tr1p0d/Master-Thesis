module DataSetParser
  where

import Control.Monad ((>=>), void)
import System.IO (IOMode(ReadMode), withFile, hGetLine, hIsEOF)

import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.Set as Set (empty, insert, member, size)

import Control.Monad.Loops (untilM)
import Data.IP (fromIPv4)

type IPDataSet = [[Int]]

-- | Returns dataset as list of IP octets
parseDataSet :: FilePath -> IO IPDataSet
parseDataSet file = withFile file ReadMode
    (\h -> untilM (readIP h) (hIsEOF h))
  where
    readIP = hGetLine >=> return . fromIPv4  . read
