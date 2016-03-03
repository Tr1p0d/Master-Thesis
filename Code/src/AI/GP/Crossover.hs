{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Crossover where

import Prelude (Float)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (Monad(return))
import Data.Foldable (length)
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((!!), any, null)
import Data.Maybe (Maybe(Just, Nothing))
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
    -> m (Maybe (GProgram op t, GProgram op t))
subtreeCrossoverPreferLeafs ps ub preference = do
    leaf <- sample $ bernoulli preference
    if leaf then subtreeCrossoverLeaf ps else subtreeCrossoverUniformNodes ps ub

subtreeCrossoverUniform
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Uppwer bound
    -> m (Maybe (GProgram op t, GProgram op t))
subtreeCrossoverUniform ps = subtreeCrossoverUniformGen ps 0

subtreeCrossoverUniformGen
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Lower bound
    -> Int -- | Uppwer bound
    -> m (Maybe (GProgram op t, GProgram op t))
subtreeCrossoverUniformGen ps lb ub =
    subtreeCrossoverGen ps (sample $ uniform lb ub) arbitraryUniform

subtreeCrossoverGen
    :: (Monad m)
    => (GProgram op t, GProgram op t)
    -> m Int
    -> ([GPZipper op t] -> m (GPZipper op t))
    -> m (Maybe (GProgram op t, GProgram op t))
subtreeCrossoverGen ps gen sel = do
    r <- gen
    let (r1, r2) = commonRegions r ps
    if any null [r1,r2]
    then return Nothing
    else (Just .) . mkProgramTuple <$> sel r1 <*> sel r2

subtreeCrossoverLeaf
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> m (Maybe (GProgram op t, GProgram op t))
subtreeCrossoverLeaf ps = subtreeCrossoverGen ps (return 0) arbitraryUniform

subtreeCrossoverUniformNodes
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Uppwer bound
    -> m (Maybe (GProgram op t, GProgram op t))
subtreeCrossoverUniformNodes ps = subtreeCrossoverUniformGen ps 1

--- <<< VARIOUS UTILITY FUNCTIONS ---------------------------------------------

commonRegions
    :: Int
    -> (GProgram op t, GProgram op t)
    -> ([GPZipper op t], [GPZipper op t])
commonRegions height = bimap' (subZippers height) . bimap' toGPZipper

arbitrary :: (Functor m) => m Int -> [a] -> m a
arbitrary selector list = (list !!) <$> selector

arbitraryUniform :: (MonadRandom m) => [a] -> m a
arbitraryUniform list = arbitrary selector list
  where
    selector = sample $ uniform 0 ((length list) -1)

--- >>> VARIOUS UTILITY FUNCTIONS ---------------------------------------------
