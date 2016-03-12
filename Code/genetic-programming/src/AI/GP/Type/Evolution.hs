{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Evolution where

import Prelude ((+), Double)

import Control.Applicative (Applicative)
import Control.Monad (Monad((>>)))
import Data.Function (($))
import Data.Functor (Functor)
import Data.Int (Int)

import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.State.Lazy (StateT{-, get, put-}, modify, {-evalStateT-})
import qualified Data.Vector as V (Vector)

import Control.Lens ((.~), (%~), makeLenses)
--import Data.Default (Default(def))
import System.Random.MWC (Gen)

import AI.GP.Type.Fitnesse (EvaluatedIndividual)
import AI.GP.Type.GProgram (Individual)

data EvolutionProgress op t = EvolutionProgress
    { _fitterIndividual :: EvaluatedIndividual op t
    , _inGeneration :: Int
    }
makeLenses ''EvolutionProgress

data EvolutionState op t = EvolutionState
    { _generation :: Int
    , _population :: V.Vector (EvaluatedIndividual op t)
    , _selection :: V.Vector (Individual op t)
    , _breed :: V.Vector (Individual op t)
    , _mutation :: V.Vector (Individual op t)
    , _randomToken :: Gen RealWorld
    , _bestScore :: Double
    , _evolutionProgress :: [EvolutionProgress op t]
    }
makeLenses ''EvolutionState

newtype Evolution op t m a =
    Evolution { runEvolution :: StateT (EvolutionState op t) m a }
    deriving (Functor, Applicative, Monad, MonadTrans)

type EvolutionResult op t = V.Vector (EvaluatedIndividual op t)

incrementGeneration :: (Monad m) => Evolution op t m ()
incrementGeneration = Evolution $ modify (generation %~ (+1))

putProgress
    :: (Monad m)
    => EvaluatedIndividual op t
    -> Evolution op t m ()
putProgress gen = Evolution $ modify (evolutionProgress.individual .~ gen)

putGeneration
    :: (Monad m)
    => V.Vector (EvaluatedIndividual op t)
    -> Evolution op t m ()
putGeneration gen = Evolution $ modify (population .~ gen)

nextGeneration
    :: (Monad m)
    => Evolution op t m (EvolutionResult op t)
    -> Evolution op t m (EvolutionResult op t)
nextGeneration next = incrementGeneration >> next
