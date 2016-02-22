{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.EvolutionState where

import Prelude ((+), Int)

import Control.Lens ((%~), makeLenses)

data EvolutionState = EvolutionState
    { _getGeneration :: Int
--  , _getFittest :: Double
    }
makeLenses ''EvolutionState

newGeneration :: EvolutionState -> EvolutionState
newGeneration = getGeneration %~ (+1)
