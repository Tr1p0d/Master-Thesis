{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Crossover where

import Prelude (Float)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (return)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List ((!!))
import Data.Foldable (length)
import GHC.Num ((-))

import AI.GP.Type.GProgram (GProgram)
import AI.GP.Type.GPZipper (GPZipper, mkProgramTuple, subZippers, toGPZipper)
import Data.Bifunctor.Extended (bimap')
import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Bernoulli (bernoulli)
import Data.Random.Distribution.Uniform (uniform)


subtreeCrossoverPreferLeafs
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int      -- | Uppwer bound
    -> Float    -- | percentil of Leaf preference
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverPreferLeafs ps ub preference = do
    leaf <- sample $ bernoulli preference
    if leaf then subtreeCrossoverLeaf ps else subtreeCrossoverUniformNodes ps ub

subtreeCrossoverUniform
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Uppwer bound
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniform ps = subtreeCrossoverUniformGen ps 0

subtreeCrossoverUniformGen
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Lower bound
    -> Int -- | Uppwer bound
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniformGen ps lb ub =
    subtreeCrossoverGen ps $ sample $ uniform lb ub

subtreeCrossoverGen
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> m Int
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverGen ps g = do
    r <- g
    let (r1, r2) = commonRegions r ps
    mkProgramTuple <$> arbitrary' r1  <*> arbitrary' r2

subtreeCrossoverLeaf
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverLeaf ps = subtreeCrossoverGen ps $ return 0

subtreeCrossoverUniformNodes
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Uppwer bound
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniformNodes ps = subtreeCrossoverUniformGen ps 1

--- <<< VARIOUS UTILITY FUNCTIONS ---------------------------------------------

commonRegions
    :: Int
    -> (GProgram op t, GProgram op t)
    -> ([GPZipper op t], [GPZipper op t])
commonRegions height = bimap' (subZippers height) . bimap' toGPZipper

arbitrary :: (MonadRandom m) => [a] -> Int -> m a
arbitrary list elems = (list !!) <$> sample (uniform 0 $ elems - 1)

arbitrary' :: (MonadRandom m) => [a] -> m a
arbitrary' list = arbitrary list (length list)

--- >>> VARIOUS UTILITY FUNCTIONS ---------------------------------------------
