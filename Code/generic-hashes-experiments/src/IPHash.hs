{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad (void)
import Data.Bits (Bits, rotateR, xor)
import Data.Word (Word32)

import qualified Data.Set as Set (Set, empty, insert, size)

import DataSets (dataSets8k)
import DataSetParser (IPDataSet)
import Utils (ipToByteString, loadFactorExperiment)

main :: IO ()
main = void $ mapM (loadFactorExperiment "IPHash" evalDataSet) dataSets8k

evalDataSet :: IPDataSet -> Int
evalDataSet = Set.size . foldl eval Set.empty

eval set ip = Set.insert (iphash' ip `mod` 8192) set

iphash :: [Int] -> Int
iphash [o1,o2,o3,o4] = ((o1 + (4219 * (2213 + o3))) + (xor o4 ((rotateR ((3259 + o1) + (2039 + o1)) o2) * o2)))

iphash' :: [Int] -> Word32
iphash' [o0i,o1i,o2i,o3i] = (xor ((+) (xor ((+) o3 ((*) 2897 o2)) o0) o1) ((*) ((*) (rotateR 107 653) (xor o0 3461)) ((*) (rotateR 107 653) (xor o0 3461))))
  where
   o0 = fromIntegral o0i
   o1 = fromIntegral o1i
   o2 = fromIntegral o2i
   o3 = fromIntegral o3i

rotateR' :: (Bits a) => a -> Word32 -> a
rotateR' v rot = rotateR v rot'
  where
    rot' = fromIntegral rot
