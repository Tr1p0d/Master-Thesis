{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Population
    ( GPPopulation
    , _getPopulation
    , crossPopulation
    , emptySelection
    , getArbitraryIndividual
    , getArbitraryPair
    , getPopulation
    , mergeGeneration
    , mkBreed
    , mkEvalued
    , mkGeneration
    , mkGenerationZero
    , mkInitial
    , mkMuted
    , mkSelection
    , mutePopulation
    , populationLength
    , replenishPopulation
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
import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Uniform (uniform)

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


getArbitraryPair
    :: (MonadRandom m)
    => SelectionPopulation op t
    -> m (IndividualPair op t)
getArbitraryPair population = (,)
    <$> arbitraryVector selector barePop
    <*> arbitraryVector selector barePop
  where
    barePop = population ^. getPopulation
    selector = sample $ uniform 0 (V.length barePop - 1)

getArbitraryIndividual :: (MonadRandom m) => GPPopulation a e -> m e
getArbitraryIndividual =
    arbitraryUniformVector . (^. getPopulation)

populationLength :: GPPopulation a e -> Int
populationLength = length . _getPopulation

mergeGeneration
    :: GPPopulation a e
    -> GPPopulation b e
    -> GPPopulation 'Generation e
mergeGeneration a b = mkGeneration $ _getPopulation a V.++ _getPopulation b

mutePopulation
    :: (MonadRandom m)
    => BreedPopulation op t
    -> Float -- ^ Mutation probability
    -> (GProgram op t -> m (GProgram op t))
    -> m (MutedPopulation op t)
mutePopulation breed probability method =
    mkMuted <$> mapM stdMute' (breed ^. getPopulation)
  where
    stdMute' = stdMute probability method

crossPopulation
    :: (MonadRandom m)
    => SelectionPopulation op t
    -> Int -- ^ The population size
    -> (IndividualPair op t -> m (IndividualPair op t))
    -> m (BreedPopulation op t)
crossPopulation selection size cross
    | even size = mkBreed . flatten <$> cross' (size `div` 2)
    | otherwise = mkBreed . V.drop 1 . flatten <$> (cross' $ (size `div` 2) + 1)
  where
    cross' s = V.replicateM
        s
        (getArbitraryPair selection >>= cross)

replenishPopulation
    :: (MonadRandom m)
    => (EvaluedPopulation op t -> m (GProgram op t))
    -> EvaluedPopulation op t
    -> MutedPopulation op t
    -> m (Generation op t)
replenishPopulation replenish evalued muted =
    mergeGeneration muted <$> replenished
  where
    replenished = mkGeneration <$> V.replicateM rSize (replenish evalued)
    rSize = populationLength evalued - populationLength muted
