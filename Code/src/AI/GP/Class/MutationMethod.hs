{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Class.MutationMethod where

import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.Population (Population)
import AI.GP.Class.Program (Program)
import AI.GP.Type.Population (GPPopulation)
import AI.GP.Type.PopulationType (PopulationType(Breed, Muted))

class MutationMethod mut where
    mutate
        :: (MonadRandom m, Population p, Program e)
        => mut
        -> GPPopulation p 'Breed e
        -> m (GPPopulation p 'Muted e)

