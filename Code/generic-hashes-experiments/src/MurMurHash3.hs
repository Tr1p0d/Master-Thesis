{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad (void)

import qualified Data.Set as Set (empty, insert, size)

import Data.Hash.Murmur (murmur3)

import DataSets (dataSets8k)
import DataSetParser (IPDataSet)
import Utils (ipToByteString, loadFactorExperiment)

main :: IO ()
main = void $ mapM (loadFactorExperiment "MurMurHash3" evalDataSet) dataSets8k

evalDataSet :: IPDataSet -> Int
evalDataSet = Set.size . foldl eval Set.empty

eval set ip = Set.insert (murmur3 12345 (ipToByteString ip) `mod` 8192) set
