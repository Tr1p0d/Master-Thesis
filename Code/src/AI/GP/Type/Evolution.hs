{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Evolution where

import Prelude ((+))

import Control.Applicative (Applicative)
import Control.Monad (Monad((>>)))
import Data.Function (($))
import Data.Functor (Functor)
import Data.Int (Int)

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.State.Lazy (StateT, put, modify)
import qualified Data.Vector as V (Vector)

import Control.Lens ((%~), makeLenses)

import AI.GP.Type.Fitnesse (Fitness)
import AI.GP.Type.GProgram (GProgram)


newtype EvolutionState = EvolutionState
    { _generation :: Int
    }
makeLenses ''EvolutionState

newtype Evolution m a = Evolution { runEvolution :: StateT EvolutionState m a }
    deriving (Functor, Applicative, Monad, MonadTrans)

type EvolutionResult op t = V.Vector (Fitness (GProgram op t))

startEvolution :: (Monad m) => Evolution m ()
startEvolution = Evolution $ put $ EvolutionState 0

incrementGeneration :: (Monad m) => Evolution m ()
incrementGeneration = Evolution $ modify (generation %~ (+1))

nextGeneration
    :: (Monad m)
    => Evolution m (EvolutionResult op t)
    -> Evolution m (EvolutionResult op t)
nextGeneration next = incrementGeneration >> next
