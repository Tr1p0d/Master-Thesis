{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population
    ( GPPopulation
    , evalPopulation
    , mkGenerationZero
    , mkInitial
    , mkMuted
    , mkSelection
    )
  where

import Control.Applicative ((<$>))
import Control.Monad (Monad)
import Data.Function ((.))

import Control.Lens (makeLenses)
import qualified Data.Vector as V (Vector, mapM)

import AI.GP.Type.Fitnesse (Fitness)
import AI.GP.Type.GProgram (GProgram)
import AI.GP.Type.PopulationType
    ( PopulationType(Evaluated, Generation, Initial, Muted, Selection)
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

evalPopulation
    :: (Monad m)
    => (GProgram op t -> m (Fitness (GProgram op t)))
    -> GPPopulation 'Generation (GProgram op t)
    -> m (GPPopulation 'Evaluated (Fitness (GProgram op t)))
evalPopulation eval = (GPPopulation <$>) . V.mapM eval . _getPopulation

populationArbitrary
