{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad (void)

import qualified Data.Set as Set (empty, insert, size)

import FarmHash (hash)

import DataSets (dataSets8k)
import DataSetParser (IPDataSet)
import Utils (ipToByteString, loadFactorExperiment)

main :: IO ()
main = void $ mapM (loadFactorExperiment "FarmHash" evalDataSet) dataSets8k

evalDataSet :: IPDataSet -> Int
evalDataSet = Set.size . foldl eval Set.empty

eval set ip = Set.insert (hash (ipToByteString ip) `mod` 8192) set
