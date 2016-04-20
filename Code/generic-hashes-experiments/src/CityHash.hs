{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad (void)

import qualified Data.Set as Set (Set, empty, insert, size)

import Data.Digest.CityHash (cityHash64)

import DataSets (dataSets8k)
import DataSetParser (IPDataSet)
import Utils (ipToByteString, loadFactorExperiment)

main :: IO ()
main = void $ mapM (loadFactorExperiment "CityHash" evalDataSet) dataSets8k

evalDataSet :: IPDataSet -> Int
evalDataSet = Set.size . foldl eval Set.empty

eval set ip = Set.insert (cityHash64 (ipToByteString ip) `mod` 8192) set
