{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Fitnesse where

import Prelude (Double)

import Data.Bool (Bool(False))

import Control.Lens (makeLenses)

import AI.GP.Type.GProgram (GProgram)

data Fitness e = Fitness
    { _getIndividual :: e
    , _getScore :: Double
    , _fitEnough :: Bool
    }
makeLenses ''Fitness

emptyFitness :: GProgram op t -> Fitness (GProgram op t)
emptyFitness p = Fitness p 0.0 False

discardFitness :: Fitness e -> e
discardFitness =  _getIndividual

mkFitness :: e -> Double -> Bool -> Fitness e
mkFitness = Fitness
