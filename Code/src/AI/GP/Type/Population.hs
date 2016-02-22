{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population where

import Control.Lens (makeLenses)

import AI.GP.Type.PopulationType
    ( PopulationType(Generation, Initial, Selection)
    )
import AI.GP.Type.Fitnesse (Fitness)

data GPPopulation p (t :: PopulationType) e = GPPopulation
    { _getPopulation :: p e
    }
makeLenses ''GPPopulation

data GPEvaluatedPopulation p e = GPEvaluatedPopulation
    { _getEvalutedPopulation :: p (Fitness e)
    }
makeLenses ''GPEvaluatedPopulation

mkInitial :: p e -> GPPopulation p 'Initial e
mkInitial = GPPopulation

mkSelection :: p e -> GPPopulation p 'Selection e
mkSelection = GPPopulation

mkGenerationZero :: GPPopulation p 'Initial e -> GPPopulation p 'Generation e
mkGenerationZero (GPPopulation p) = GPPopulation p
