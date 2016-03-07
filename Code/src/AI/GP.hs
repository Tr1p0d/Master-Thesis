{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP where

import Prelude (div, (-))

import Control.Applicative ((<$>))
import Control.Monad (Monad((>>=), return), mapM)
--import Data.Foldable (any)
import Data.Bool (not)
import Data.Function (($), (.))
import Data.Random (MonadRandom)

import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (replicate, replicateM, filter, null)

import Control.Lens ((^.))
--import Data.Random.Distribution.Uniform (uniform)

import AI.GP.Mutation (stdMute)
import AI.GP.Type.Evolution
    ( Evolution
    , EvolutionResult
    , nextGeneration
    , startEvolution
    )
import AI.GP.Type.Fitnesse (evalIndividual, fitEnough)
import AI.GP.Type.GPEvolutionParams
    ( GPEvolutionParams
    , crossoverMethod
    , fitness
    , initMethod
    , mutationMethod
    , mutationProbability
    , populationSize
    , populationSize
    , replenishMethod
    , selectionMethod
    , terminate
    )
import AI.GP.Type.Population
    ( Generation
    , InitialPopulation
    , evalPopulation
    , getPopulation
    , mergeGeneration
    , mkBreed
    , mkGeneration
    , mkGenerationZero
    , mkInitial
    , mkMuted
    , populationLength
    )
import AI.GP.Utils (getArbitraryPair, flatten)

evolve
    :: (MonadRandom m)
    => GPEvolutionParams m op t
    -> Evolution m (EvolutionResult op t)
evolve setup = do
    initialPopulation <- mkInitialPopulation
    evolveWithInitPopulation setup initialPopulation
  where
    mkInitialPopulation = mkInitial <$> lift (V.replicateM
        (setup ^. populationSize)
        (setup ^. initMethod))

evolveWithInitPopulation
    :: (MonadRandom m)
    => GPEvolutionParams m op t
    -> InitialPopulation op t
    -> Evolution m (EvolutionResult op t)
evolveWithInitPopulation setup population = do
    startEvolution
    evolveWith setup (mkGenerationZero population)

evolveWith
    :: (MonadRandom m)
    => GPEvolutionParams m op t
    -> Generation op t
    -> Evolution m (EvolutionResult op t)
evolveWith setup population = do
    evalued <- lift $ evalPopulation fitness' population
    let fitEnough' = fitEnoughIndividuals evalued
    if not $ V.null fitEnough'
    then return fitEnough'
    else do
        selection <- lift $ (setup ^. selectionMethod) evalued
        breed <- lift $ mkBreed <$> crossover selection
        lift $ mkMuted <$> mute breed
        >>= lift . replenish evalued
        >>= nextGeneration . evolveWith setup
  where
    fitness' = evalIndividual (setup ^. fitness) (setup ^. terminate)
    fitEnoughIndividuals = V.filter (^. fitEnough) . ( ^. getPopulation)

    crossover selection = flatten <$> V.replicateM
        (setup ^. populationSize `div` 2)
        (getArbitraryPair selection >>= setup ^. crossoverMethod)

    mute breed = mapM stdMute' (breed ^. getPopulation)
      where
        stdMute' = stdMute
            (setup ^. mutationProbability)
            (setup ^. mutationMethod)

    replenish evalued muted = return $ mergeGeneration muted replenished
      where
        replenished = mkGeneration
            $ V.replicate replenishSize
            $ (setup ^. replenishMethod) evalued

        replenishSize = populationLength evalued - populationLength muted
