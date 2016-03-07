{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population
    ( GPPopulation
    , emptySelection
    , evalPopulation
    , getPopulation
    , mergeGeneration
    , mkBreed
    , mkGeneration
    , mkGenerationZero
    , mkInitial
    , mkMuted
    , mkSelection
    , populationLength
    -- Type aliases
    , BreedPopulation
    , EvaluedPopulation
    , Generation
    , InitialPopulation
    , MutedPopulation
    , SelectionPopulation
    -- Type aliases
    )
  where

import Control.Applicative ((<$>))
import Control.Monad (Monad)
import Data.Int (Int)
import Data.Foldable (length)
import Data.Function (($), (.))

import Control.Lens (makeLenses)
import qualified Data.Vector as V (Vector, (++), mapM, empty)

import AI.GP.Type.Fitnesse (Fitness)
import AI.GP.Type.GProgram (GProgram)
import AI.GP.Type.PopulationType
    ( PopulationType(Breed, Evaluated, Generation, Initial, Muted, Selection)
    )

data GPPopulation (t :: PopulationType) e = GPPopulation
    { _getPopulation :: V.Vector e
    }
makeLenses ''GPPopulation

type BreedPopulation op t = GPPopulation 'Muted (GProgram op t)
type EvaluedPopulation op t =
    GPPopulation 'Evaluated (Fitness (GProgram op t))
type Generation op t = GPPopulation 'Generation (GProgram op t)
type MutedPopulation op t = GPPopulation 'Muted (GProgram op t)
type SelectionPopulation op t = GPPopulation 'Selection (GProgram op t)
type InitialPopulation op t = GPPopulation 'Initial (GProgram op t)

mkInitial :: V.Vector e -> GPPopulation 'Initial e
mkInitial = GPPopulation

mkSelection :: V.Vector e -> GPPopulation 'Selection e
mkSelection = GPPopulation

emptySelection :: GPPopulation 'Selection e
emptySelection = GPPopulation V.empty

mkBreed :: V.Vector e -> GPPopulation 'Breed e
mkBreed = GPPopulation

mkMuted :: V.Vector e -> GPPopulation 'Muted e
mkMuted = GPPopulation

mkGeneration :: V.Vector e -> GPPopulation 'Generation e
mkGeneration = GPPopulation

mkGenerationZero :: GPPopulation 'Initial e -> GPPopulation 'Generation e
mkGenerationZero (GPPopulation p) = GPPopulation p

evalPopulation
    :: (Monad m)
    => (GProgram op t -> m (Fitness (GProgram op t)))
    -> GPPopulation 'Generation (GProgram op t)
    -> m (GPPopulation 'Evaluated (Fitness (GProgram op t)))
evalPopulation eval = (GPPopulation <$>) . V.mapM eval . _getPopulation

populationLength :: GPPopulation a e -> Int
populationLength = length . _getPopulation

mergeGeneration
    :: GPPopulation a e
    -> GPPopulation b e
    -> GPPopulation 'Generation e
mergeGeneration a b = mkGeneration $ _getPopulation a V.++ _getPopulation b
