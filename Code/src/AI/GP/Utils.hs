{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Utils where

import Control.Applicative ((<$>), (<*>))
import Data.Foldable (length)
import Data.Function (($))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((!!))
import GHC.Num ((-))

import qualified Data.Vector as V (Vector, (!), cons, empty, foldl, length)

import Control.Lens ((^.))
import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Uniform (uniform)

import AI.GP.Type.GProgram (IndividualPair)
import AI.GP.Type.Population (SelectionPopulation, getPopulation)


arbitrary :: (Functor m) => m Int -> [a] -> m a
arbitrary selector list = (list !!) <$> selector

arbitraryUniform :: (MonadRandom m) => [a] -> m a
arbitraryUniform list = arbitrary selector list
  where
    selector = sample $ uniform 0 (length list - 1)

arbitraryVector :: (Functor m) => m Int -> V.Vector a -> m a
arbitraryVector selector list = (list V.!) <$> selector

arbitraryUniformVector :: (MonadRandom m) => V.Vector a -> m a
arbitraryUniformVector vec = arbitraryVector selector vec
  where
    selector = sample $ uniform 0 (length vec - 1)

getArbitraryPair
    :: (MonadRandom m)
    => SelectionPopulation op t
    -> m (IndividualPair op t)
getArbitraryPair population = (,)
    <$> arbitraryVector selector barePop
    <*> arbitraryVector selector barePop
  where
    barePop = population ^. getPopulation
    selector = sample $ uniform 0 (V.length barePop - 1)

flatten :: V.Vector (a,a) -> V.Vector a
flatten = V.foldl (\acc (a, b) -> a `V.cons` (b `V.cons` acc)) V.empty
