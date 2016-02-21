{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Class.BreedMethod where

import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.Program (Program)
import AI.GP.Class.Population (Population)
import AI.GP.Type.Population (GPPopulation)
import AI.GP.Type.PopulationType (PopulationType(Breed, Selection))

class BreedMethod b where
    cross
        :: (MonadRandom m, Population p, Program e)
        => b -> GPPopulation p 'Selection e -> m (GPPopulation p 'Breed e)

