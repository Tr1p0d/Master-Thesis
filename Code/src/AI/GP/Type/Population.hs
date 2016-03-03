{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population where

import Control.Lens (makeLenses)

import qualified Data.Vector as V (Vector)

import AI.GP.Type.PopulationType
    ( PopulationType(Generation, Initial, Muted, Selection)
    )

data GPPopulation (t :: PopulationType) e = GPPopulation
    { _getPopulation :: V.Vector e
    }
makeLenses ''GPPopulation

mkInitial :: V.Vector e -> GPPopulation 'Initial e
mkInitial = GPPopulation

mkSelection :: V.Vector e -> GPPopulation 'Selection e
mkSelection = GPPopulation

mkMuted :: V.Vector e -> GPPopulation 'Muted e
mkMuted = GPPopulation

mkGenerationZero :: GPPopulation 'Initial e -> GPPopulation 'Generation e
mkGenerationZero (GPPopulation p) = GPPopulation p
