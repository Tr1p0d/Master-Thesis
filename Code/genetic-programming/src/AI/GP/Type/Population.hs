{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population
    ( GPPopulation
    , _getPopulation
    , emptySelection
    , getPopulation
    , mergeGeneration
    , mkBreed
    , mkEvalued
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

import Prelude (Float, even, div, (-), (+))

import Control.Applicative ((<$>), (<*>))
import Control.Monad (Monad((>>=)), mapM)
import Data.Bool (otherwise)
import Data.Eq (Eq)
import Data.Foldable (length)
import Data.Function (($), (.))
import Data.Int (Int)
import Text.Show (Show)

import qualified Data.Vector as V
    ( Vector
    , (++)
    , drop
    , empty
    , length
    , replicateM
    )

import Control.Lens ((^.), makeLenses)

import AI.GP.Type.Fitnesse (Fitness)
import AI.GP.Type.GProgram (GProgram, IndividualPair)
import AI.GP.Type.PopulationType
    ( PopulationType(Breed, Evaluated, Generation, Initial, Muted, Selection)
    )
import AI.GP.Mutation (stdMute)
import AI.GP.Utils (flatten, arbitraryUniformVector, arbitraryVector)

data GPPopulation (t :: PopulationType) e = GPPopulation
    { _getPopulation :: V.Vector e
    }
    deriving (Eq, Show)
makeLenses ''GPPopulation

type BreedPopulation op t = GPPopulation 'Breed (GProgram op t)
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

mkEvalued :: V.Vector (Fitness e) -> GPPopulation 'Evaluated (Fitness e)
mkEvalued = GPPopulation

mkMuted :: V.Vector e -> GPPopulation 'Muted e
mkMuted = GPPopulation

mkGeneration :: V.Vector e -> GPPopulation 'Generation e
mkGeneration = GPPopulation

mkGenerationZero :: GPPopulation 'Initial e -> GPPopulation 'Generation e
mkGenerationZero (GPPopulation p) = GPPopulation p

populationLength :: GPPopulation a e -> Int
populationLength = length . _getPopulation

mergeGeneration
    :: GPPopulation a e
    -> GPPopulation b e
    -> GPPopulation 'Generation e
mergeGeneration a b = mkGeneration $ _getPopulation a V.++ _getPopulation b
