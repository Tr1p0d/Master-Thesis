{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Instance.Mutation.Point where

import Prelude (Double)

import Control.Applicative ((<$>))
import Data.Int(Int)

import Control.Lens ((^.))
import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.MutationMethod (MutationMethod(mutate))
import AI.GP.Class.Population (Population)
import AI.GP.Class.Program (Program)
import AI.GP.Type.Class.Mutation.Point
    ( Point
    , MutationDistribution
    , depth
    , mutationProbability
    , distribution
    )
import AI.GP.Type.Fitnesse (Fitness, discardFitness)
import AI.GP.Type.Population (getPopulation, mkMuted)
import AI.GP.Type.PopulationType (PopulationType(Breed, Muted))

instance MutationMethod Point where
    mutate mut breed =
        mkMuted <$> withProbability mutateIndividual prob (breed ^. getPopulation)
      where
        prob = mut ^. mutationProbability
        mutateIndividual e =
            pointMutation (mut ^. distribution) (mut ^. depth) e

withProbability :: (e -> m e) -> Double -> p e -> m (p e)
withProbability f prob p = let a=a in a

pointMutation
    :: (MonadRandom m, Program e)
    => MutationDistribution
    -> Int -- | Depth
    -> e
    -> m e
pointMutation = let a=a in a
