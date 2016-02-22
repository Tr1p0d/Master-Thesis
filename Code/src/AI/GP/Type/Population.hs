{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population where

import Control.Lens (makeLenses)

import AI.GP.Type.PopulationType (PopulationType(Generation, Initial))
import AI.GP.Type.Fitnesse (Fitnesse)

data GPPopulation p (t :: PopulationType) e = GPPopulation
    { _getPopulation :: p e
    }
makeLenses ''GPPopulation

data GPEvaluatedPopulation p e = GPEvaluatedPopulation
    { _getEvalutedPopulation :: p (Fitnesse e)
    }
makeLenses ''GPEvaluatedPopulation

mkInitial :: p e -> GPPopulation p 'Initial e
mkInitial = GPPopulation

mkGenerationZero :: GPPopulation p 'Initial e -> GPPopulation p 'Generation e
mkGenerationZero (GPPopulation p) = GPPopulation p
