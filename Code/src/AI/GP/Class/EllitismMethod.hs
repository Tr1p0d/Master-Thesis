{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Class.EllitismMethod where

import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.Population (Population)
import AI.GP.Class.Program (Program)
import AI.GP.Type.Population (GPPopulation, GPEvaluatedPopulation)
import AI.GP.Type.PopulationType (PopulationType(Muted, Generation))

class EllitismMethod el where
    replenish
        :: (MonadRandom m, Population p, Program e)
        => el
        -> GPEvaluatedPopulation p e
        -> GPPopulation p 'Muted e
        -> m (GPPopulation p 'Generation e)

