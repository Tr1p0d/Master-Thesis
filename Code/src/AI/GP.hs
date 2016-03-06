{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP where

import Prelude (Double)

--import Control.Applicative (Applicative, (<$>))
--import Control.Monad (Monad((>>=)))
--import Data.Foldable (any)
--import Data.Function (($), (.))
--import Data.Functor (Functor(fmap))

import Control.Lens (makeLenses)
import Data.Int (Int)
import Data.Maybe (Maybe)

import AI.GP.Type.GProgram (GProgram)
import AI.GP.Type.Population (EvaluedPopulation, SelectionPopulation)

data GPEvolutionSetup m op t = GPEvolutionSetup
    { _populationSize :: Int
    , _initMethod :: m (GProgram op t)
    , _mutationMethod :: m (Maybe (GProgram op t))
    , _crossoverMethod :: m (Maybe (GProgram op t, GProgram op t))
    , _selectionMethod
        :: m (EvaluedPopulation op t -> m (SelectionPopulation op t))
    , _fitness :: GProgram op t -> m Double
    }
makeLenses ''GPEvolutionSetup

evolve
    :: GPEvolutionSetup m op t
    -> Int
evolve  = 1
