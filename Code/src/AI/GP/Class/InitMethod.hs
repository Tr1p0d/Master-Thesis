{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Class.InitMethod where

import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.Population (Population)
import AI.GP.Class.Program (Program)
import AI.GP.Type.Population (GPPopulation)
import AI.GP.Type.PopulationType (PopulationType(Initial))

class InitMethod i where
    init
        :: (MonadRandom m, Population p, Program e)
        => i -> m (GPPopulation p 'Initial e)
