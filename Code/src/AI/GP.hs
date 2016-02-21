{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP where

import Prelude ((+))

import Control.Applicative ((<$>), Applicative)
import Control.Monad (Monad, (>>=), return)
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.Traversable (mapM)

import Control.Monad.Trans.State (StateT, withStateT)

--import Control.Monad.Loops (iterateUntil)
--import Control.Monad.Random (getRandom, getRandomR)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Random.Class (MonadRandom)
import Control.Lens ((^.), (%~))

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
    )
import AI.GP.Type.EvolutionState (EvolutionState, getGeneration)
import AI.GP.Type.Population
    ( GPEvaluatedPopulation(GPEvaluatedPopulation)
    , GPPopulation
    , getPopulation
    , mkGenerationZero
    )
import AI.GP.Type.PopulationType (PopulationType(Generation, Initial))

newtype Evolution m a = Evolution {runEvolution :: StateT EvolutionState m a}
    deriving (Functor, Applicative, Monad, MonadRandom, MonadTrans)

--fullMethod :: (MonadRandom m) => Int -> m E
--fullMethod 0 = Const <$> getRandom
--fullMethod n = sample [Plus] <*> subExpr <*> subExpr
--  where
--    subExpr = fullMethod $ n - 1

--tournament
--    :: (MonadRandom m)
--    => Int -- | Tournament rounds
--    -> Int -- | Tournament size
--    -> V.Vector (Double, e)
--    -> m (V.Vector e)
--tournament _r s ep = V.replicateM s (randomElem ep)

evolve
    ::  ( BreedMethod b
        , EllitismMethod el
        , InitMethod i
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        , MonadRandom m
        )
    => EvolutionParams i s b mut el p e -> Evolution m a
evolve e = (init $ e ^. initMethod) >>= evolveWithInitialPopulation e

evolveWithInitialPopulation
    ::  ( BreedMethod b
        , EllitismMethod el
        , InitMethod i
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        , MonadRandom m
        )
    => EvolutionParams i s b mut el p e
    -> GPPopulation p 'Initial e
    -> Evolution m a
evolveWithInitialPopulation e = evolveWith e . mkGenerationZero

evolveWith
    ::  ( BreedMethod b
        , EllitismMethod el
        , InitMethod i
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        , MonadRandom m
        )
    => EvolutionParams i s b mut el p e
    -> GPPopulation p 'Generation e
    -> Evolution m a
evolveWith e p = do
    evaluated <-  GPEvaluatedPopulation <$> mapM eval' (p ^. getPopulation)
    selection <- select (e ^. selectionMethod) evaluated
    breed <- cross (e ^. breedMethod) selection
    offsprings <- mutate (e ^. mutationMethod) breed
    nextGen <- replenish (e ^. ellitismMethod) evaluated offsprings
    continueEvolution nextGen
  where
    eval' prog = return (eval prog, prog)
    --continueEvolution = do
    --    lift $ modify incrementGeneration
    -- | get rid of this ugliness later
    continueEvolution ng =
        Evolution $ withStateT incrementGeneration
            $ runEvolution $ evolveWith e ng
    incrementGeneration = getGeneration %~ (+1)
