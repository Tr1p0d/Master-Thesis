{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Crossover where

import Prelude (Double, error)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (Monad(return))
import Data.Function (($))
import Data.Int (Int)
import Data.List (any, null)
import Data.Ord (min)

import Control.Monad.Primitive (PrimMonad, PrimState)

import System.Random.MWC (Gen, uniformR)
import System.Random.MWC.Distributions (bernoulli)

import AI.GP.Type.GProgram (GProgram, _height)
import AI.GP.Type.GPZipper
    ( GPZipper
    , commonRegions
    , mkProgramTuple
    )
import AI.GP.Utils (arbitraryUniform)


subtreeCrossoverPreferLeafs
    :: (PrimMonad m)
    => Double -- | percentil of Leaf preference
    -> Gen (PrimState m)
    -> (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverPreferLeafs preference token progPair = do
    leaf <- bernoulli preference token
    if leaf
    then subtreeCrossoverLeaf token progPair
    else subtreeCrossoverUniformNodes token progPair

subtreeCrossoverUniform
    :: (PrimMonad m)
    => Gen (PrimState m)
    -> (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniform token progPair@(p1,p2) =
    subtreeCrossoverUniformGen 0 (min (_height p1) (_height p2)) token progPair

subtreeCrossoverUniformGen
    :: (PrimMonad m)
    => Int -- | Lower bound
    -> Int -- | Uppwer bound
    -> Gen (PrimState m)
    -> (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniformGen lb ub token progPair =
    subtreeCrossoverGen progPair (uniformR (lb, ub) token) (arbitraryUniform token)

subtreeCrossoverGen
    :: (Monad m)
    => (GProgram op t, GProgram op t)
    -> m Int
    -> ([GPZipper op t] -> m (GPZipper op t))
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverGen ps gen sel = do
    r <- gen
    let (r1, r2) = commonRegions r ps
    if any null [r1,r2]
    then error "Empty common region"
    else mkProgramTuple <$> sel r1 <*> sel r2

subtreeCrossoverLeaf
    :: (PrimMonad m)
    => Gen (PrimState m)
    -> (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverLeaf token progPair =
    subtreeCrossoverGen progPair (return 0) $ arbitraryUniform token

subtreeCrossoverUniformNodes
    :: (PrimMonad m)
    => Gen (PrimState m)
    -> (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniformNodes token progPair@(p1,p2) =
    subtreeCrossoverUniformGen 1 (min (_height p1) (_height p2)) token progPair
