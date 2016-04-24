{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP
    ( Evolution
    , GPEvolutionParams(GPEvolutionParams)
    , evalEvolution
    , evolve
    , runEvolution
    )
  where

import Control.Monad (Monad(return))
import Data.Bool (Bool)
import Data.Function (($), (.))
import Text.Show (Show)

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (gets)
import qualified Data.Vector as V
    ( replicateM
    )

import Control.Lens ((^.))

import AI.GP.Type.Evolution
    ( Evolution(Evolution, runEvolution)
    , _randomToken
    , cross
    , debugState
    , evalEvolution
    , evalPopulation
    , eventuallyPutProgress
    , incrementGeneration
    , mute
    , putProgress
    , replenish
    , select
    , terminationCondition
    )
import AI.GP.Type.GPEvolutionParams
    ( GPEvolutionParams(GPEvolutionParams)
    , crossoverMethod
    , fitness
    , initMethod
    , maxGenerations
    , mutationMethod
    , mutationProbability
    , populationSize
    , breedSize
    , replenishMethod
    , selectionMethod
    , terminate
    )


evolve
    ::  ( PrimMonad m
        , Show op
        , Show t
        )
    => GPEvolutionParams m op t
    -> Evolution op t m ()
evolve setup = do
    token <- Evolution $ gets _randomToken
    initial <- lift $ mkInitialPopulation token
    evalPopulation (setup ^. fitness) (setup ^. terminate) initial
    putProgress
    evolveWith setup
  where
    mkInitialPopulation = V.replicateM
        (setup ^. populationSize)
        . (setup ^. initMethod)

evolveWith
    ::  ( PrimMonad m
        , Show op
        , Show t
        )
    => GPEvolutionParams m op t
    -> Evolution op t m ()
evolveWith setup = evolutionLoop
    (terminationCondition (setup ^. terminate) (setup ^. maxGenerations)) $ do
    token <- Evolution $ gets _randomToken
    select $ (setup ^. selectionMethod) token
    cross (setup ^. breedSize) $ (setup ^. crossoverMethod) token
    mute (setup ^. mutationProbability) $ (setup ^. mutationMethod) token
    ng <- replenish $ (setup ^. replenishMethod) token
    evalPopulation (setup ^. fitness) (setup ^. terminate) ng
    eventuallyPutProgress
    debugState
    incrementGeneration

evolutionLoop :: (Monad m) => m Bool -> m () -> m ()
evolutionLoop cond operation = do
    c <- cond
    if c
    then return ()
    else do
        operation
        evolutionLoop cond operation
