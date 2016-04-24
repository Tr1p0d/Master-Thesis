{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.GPEvolutionParams where

import Prelude (Double)

import Data.Bool (Bool)
import Data.Int (Int)

import Control.Monad.Primitive (PrimState)
import qualified Data.Vector as V (Vector)

import Control.Lens (makeLenses)
import System.Random.MWC (Gen)

import AI.GP.Type.Fitnesse (EvaluatedIndividual)
import AI.GP.Type.GProgram (Individual, IndividualPair)


data GPEvolutionParams m op t = GPEvolutionParams
    { _populationSize :: Int
    , _initMethod
        :: Gen (PrimState m)
        -> m (Individual op t)

    , _selectionMethod
        :: Gen (PrimState m)
        -> V.Vector (EvaluatedIndividual op t)
        -> m (V.Vector (Individual op t))
    , _fitness :: Individual op t -> m Double
    , _terminate :: Double -> Bool

    , _breedSize :: Int
    , _crossoverMethod
        :: Gen (PrimState m)
        -> IndividualPair op t
        -> m (IndividualPair op t)

    , _mutationProbability :: Double
    , _mutationMethod
        :: Gen (PrimState m)
        -> Individual op t
        -> m (Individual op t)

    , _replenishMethod
        :: Gen (PrimState m)
        -> V.Vector (EvaluatedIndividual op t)
        -> m (V.Vector (Individual op t))

    , _maxGenerations :: Int

    }
makeLenses ''GPEvolutionParams
