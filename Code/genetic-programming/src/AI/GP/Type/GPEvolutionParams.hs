{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.GPEvolutionParams where

import Prelude (Double, Float)

import Data.Bool (Bool)
import Data.Int (Int)

import Control.Lens (makeLenses)

import AI.GP.Type.GProgram (Individual, IndividualPair)
import AI.GP.Type.Population
    ( EvaluedPopulation
    , SelectionPopulation
    )

data GPEvolutionParams m op t = GPEvolutionParams
    { _populationSize :: Int
    , _initMethod :: m (Individual op t)

    , _selectionMethod
        :: EvaluedPopulation op t -> m (SelectionPopulation op t)
    , _fitness :: Individual op t -> m Double
    , _terminate :: Double -> Bool

    , _breedSize :: Int
    , _crossoverMethod :: IndividualPair op t -> m (IndividualPair op t)

    , _mutationProbability :: Float
    , _mutationMethod :: Individual op t -> m (Individual op t)

    , _replenishMethod :: EvaluedPopulation op t -> m (Individual op t)

    , _maxGenerations :: Int

    }
makeLenses ''GPEvolutionParams
