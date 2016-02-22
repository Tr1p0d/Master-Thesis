{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP where

import Control.Applicative (Applicative, (<$>), (<*))
import Control.Monad (Monad((>>), (>>=), return))
import Data.Foldable (any)
import Data.Function (($), (.))
import Data.Functor (Functor(fmap))

import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.Trans.State (StateT)

import Control.Monad.Loops (iterateUntilM)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Random.Class (MonadRandom)
import Control.Lens ((^.), (.~), (&))

import AI.GP.Class.Population (Population)
import AI.GP.Class.Program (Program(eval))
import AI.GP.Class.BreedMethod (BreedMethod(cross))
import AI.GP.Class.EllitismMethod (EllitismMethod(replenish))
import AI.GP.Class.InitMethod (InitMethod(init))
import AI.GP.Class.MutationMethod (MutationMethod(mutate))
import AI.GP.Class.SelectionMethod (SelectionMethod(select))
--import AI.GP.Type.Evolution (Evolution(Evolution, runEvolution))
import AI.GP.Type.EvolutionParams
    ( EvolutionParams
    , breedMethod
    , ellitismMethod
    , initMethod
    , mutationMethod
    , selectionMethod
    , terminationCondition
    )
import AI.GP.Type.EvolutionState (EvolutionState, newGeneration)
import AI.GP.Type.Fitnesse (emptyFitness, getScore, fitEnough)
import AI.GP.Type.Population
    ( GPEvaluatedPopulation(GPEvaluatedPopulation)
    , GPPopulation
    , getEvalutedPopulation
    , getPopulation
    , mkGenerationZero
    )
import AI.GP.Type.PopulationType (PopulationType(Generation, Initial))

newtype Evolution m a = Evolution {runEvolution :: StateT EvolutionState m a}
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadRandom
        , MonadState EvolutionState
        , MonadTrans
        )

type EvolutionResult m p e = Evolution m (GPEvaluatedPopulation p e)

evolve
    ::  ( BreedMethod b
        , EllitismMethod el
        , InitMethod i
     --   , MonadState EvolutionState m
        , MonadRandom m
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        )
    => EvolutionParams i s b mut el p e
    -> EvolutionResult m p e
evolve e = init (e ^. initMethod) >>= evolveWithInitialPopulation e

evolveWithInitialPopulation
    ::  ( BreedMethod b
        , EllitismMethod el
        , InitMethod i
     --   , MonadState EvolutionState m
        , MonadRandom m
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        )
    => EvolutionParams i s b mut el p e
    -> GPPopulation p 'Initial e
    -> EvolutionResult m p e
evolveWithInitialPopulation e = evolveWith e . mkGenerationZero

evolveWith
    ::  ( BreedMethod b
        , EllitismMethod el
        , InitMethod i
    --    , MonadState EvolutionState m
        , MonadRandom m
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        )
    => EvolutionParams i s b mut el p e
    -> GPPopulation p 'Generation e
    -> EvolutionResult m p e
evolveWith e p = iterateUntilM
    isFinalPopulation
    computeNewGeneration
    $ evalGeneration p
  where
    isFinalPopulation ep = any (^. fitEnough) (ep ^. getEvalutedPopulation)
    evalGeneration p' = GPEvaluatedPopulation $ fmap eval' (p' ^. getPopulation)
    eval' prog =
        emptyFitness prog & getScore .~ fitnesse & fitEnough .~ cond
      where
        fitnesse = eval prog
        cond = e ^. terminationCondition $ fitnesse
    computeNewGeneration ep = evalGeneration <$>
        (select (e ^. selectionMethod) ep
        >>= cross (e ^. breedMethod)
        >>= mutate (e ^. mutationMethod)
        >>= replenish (e ^. ellitismMethod) ep
        {-<* mkNewGeneration-})
    --mkNewGeneration = modify newGeneration
