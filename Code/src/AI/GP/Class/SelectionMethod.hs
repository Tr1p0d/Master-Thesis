{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Class.SelectionMethod where

import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.Population (Population)
import AI.GP.Class.Program (Program)
import AI.GP.Type.Population (GPPopulation, GPEvaluatedPopulation)
import AI.GP.Type.PopulationType (PopulationType(Selection))

class SelectionMethod s where
    select
        :: (MonadRandom m, Population p, Program e)
        => s -> GPEvaluatedPopulation p e -> m (GPPopulation p 'Selection e)
