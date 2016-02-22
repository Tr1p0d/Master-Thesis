{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Fitnesse where

import Prelude (Double)

import Data.Bool (Bool(False))

import Control.Lens (makeLenses)

data Fitnesse e = Fitnesse
    { _getIndividual :: e
    , _getScore :: Double
    , _fitEnough :: Bool
    }
makeLenses ''Fitnesse

emptyFitness :: e -> Fitnesse e
emptyFitness e = Fitnesse e 0.0 False

mkFitness :: e -> Double -> Bool -> Fitnesse e
mkFitness = Fitnesse
