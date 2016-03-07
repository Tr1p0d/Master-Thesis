{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.GPEvolutionParams where

import Prelude (Double, Float)

import Data.Bool (Bool)
import Data.Int (Int)

import Control.Lens (makeLenses)

import AI.GP.Type.GProgram (GProgram, IndividualPair)
import AI.GP.Type.Population
    ( EvaluedPopulation
    , SelectionPopulation
    )

data GPEvolutionParams m op t = GPEvolutionParams
    { _populationSize :: Int
    , _initMethod :: m (GProgram op t)

    , _selectionMethod
        :: EvaluedPopulation op t -> m (SelectionPopulation op t)
    , _fitness :: GProgram op t -> m Double
    , _terminate :: Double -> Bool

    , _breedSize :: Int
    , _crossoverMethod :: IndividualPair op t -> m (IndividualPair op t)

    , _mutationProbability :: Float
    , _mutationMethod :: GProgram op t -> m (GProgram op t)

    , _replenishMethod :: EvaluedPopulation op t -> GProgram op t

    }
makeLenses ''GPEvolutionParams
