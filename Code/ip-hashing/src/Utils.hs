-- |
-- Module      : Utils
-- Description : The ip hash utility functions.
-- Copyright   : (c) Marek Kidon, 2016
-- License     : BSD
-- Maintainer  : xkidon00@stud.fit.vutbr.cz
-- Stability   : experimental
-- Portability :
--
-- This module contains various utility functions
-- such as parser, evaluation functoin, etc..
--
module Utils where

import Control.Monad ((>=>))
import System.IO (IOMode(ReadMode), withFile, hGetLine, hIsEOF)

import qualified Data.Vector as V (Vector, fromList)

import Control.Monad.Loops (untilM)
import Data.IP

readAdresses :: FilePath -> IO ([[Int]])
readAdresses file = withFile file ReadMode
    (\h -> untilM (readIP h) (hIsEOF h))
  where
    readIP = hGetLine >=> return . fromIPv4  . read
