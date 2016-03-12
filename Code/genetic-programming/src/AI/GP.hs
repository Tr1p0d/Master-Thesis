{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP
    ( Evolution
    , GPEvolutionParams(GPEvolutionParams)
    , evolve
    , runEvolution
    )
  where

import Prelude (Double)

import Control.Applicative ((<$>))
import Control.Monad (Monad((>>=), (>>)), (<=<) ,return)
import Data.Bool (Bool, not)
import Data.Function (($), (.))
import Text.Show (Show)

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V
    ( Vector
    , filter
    , mapM
    , null
    , replicateM
    , toList
    )
import qualified Data.Vector.Generic as IV (unsafeThaw, unsafeFreeze)

import Control.Lens ((^.))
import qualified Data.Vector.Algorithms.Merge as Merge (sort)

import AI.GP.Type.Evolution
    ( Evolution(runEvolution)
    , putGeneration
    )
import AI.GP.Type.Fitnesse (EvaluatedIndividual, evalIndividual, fitEnough)
import AI.GP.Type.GPEvolutionParams
    ( GPEvolutionParams(GPEvolutionParams)
    , crossoverMethod
    , fitness
    , initMethod
    , mutationMethod
    , mutationProbability
    , populationSize
    , breedSize
    , replenishMethod
    , selectionMethod
    , terminate
    )
import AI.GP.Type.GProgram (Individual)

evolve
    :: (PrimMonad m)
    => GPEvolutionParams m op t
    -> Evolution op t m ()
evolve setup = do
    initial <- lift mkInitialPopulation
    evalued <- lift $ evalPopulation (setup ^. fitness) (setup ^. terminate) initial
    sorted <- lift $ sortPopulation evalued
    putGeneration sorted
    evolveWith setup
  where
    mkInitialPopulation = V.replicateM
        (setup ^. populationSize)
        (setup ^. initMethod)

evolveWith
    :: (Monad m)
    => GPEvolutionParams m op t
    -> Evolution op t m ()
evolveWith setup = do
    if
--    evalued <- lift $ evalPopulation fitness' population
--    let fitEnough' = fitEnoughIndividuals evalued
--    if not $ V.null fitEnough'
--    then return fitEnough'
--    else do
--        saveFittest $ head $ sort $ V.toList $ _getPopulation evalued
--        selection <- lift $ (setup ^. selectionMethod) evalued
--        breed <- lift $ crossPopulation
--            selection
--            (setup ^. breedSize)
--            (setup ^. crossoverMethod)
--        muted <- lift $ mutePopulation breed
--            (setup ^. mutationProbability) (setup ^. mutationMethod)
--        nGen <- lift $ replenishPopulation
--            (setup ^. replenishMethod) evalued muted
--    --    traceCurrentGeneration
--        incrementGeneration
--        evolveWith setup nGen
--  where
--    fitEnoughIndividuals = V.filter (^. fitEnough) . ( ^. getPopulation)

evantuallyPutProgress ::

evalPopulation
    :: (Monad m)
    => (Individual op t -> m Double)
    -> (Double -> Bool)
    -> V.Vector (Individual op t)
    -> m (V.Vector (EvaluatedIndividual op t))
evalPopulation eval tCond = V.mapM (evalIndividual eval tCond)

sortPopulation
    :: (PrimMonad m)
    => V.Vector (EvaluatedIndividual op t)
    -> m (V.Vector (EvaluatedIndividual op t))
sortPopulation iv = do
    mv <- IV.unsafeThaw iv
    Merge.sort mv
    IV.unsafeFreeze mv
