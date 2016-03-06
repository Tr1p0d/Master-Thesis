{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP where

import Prelude (Double)

import Control.Applicative (Applicative, (<$>))
import Control.Monad (Monad((>>=), return))
--import Data.Foldable (any)
import Data.Function (($), (.))
import Data.Functor (Functor(fmap))

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Lazy (StateT, put)
import qualified Data.Vector as V (replicateM)

import Control.Lens ((^.), makeLenses)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Uniform (uniform)

import AI.GP.Type.GProgram (GProgram)
import AI.GP.Type.Population
    ( EvaluedPopulation
    , Generation
    , InitialPopulation
    , SelectionPopulation
    , mkGenerationZero
    , mkInitial
    )

data GPEvolutionSetup m op t = GPEvolutionSetup
    { _populationSize :: Int
    , _initMethod :: m (GProgram op t)
    , _mutationMethod :: m (Maybe (GProgram op t))
    , _crossoverMethod :: m (Maybe (GProgram op t, GProgram op t))
    , _selectionMethod
        :: m (EvaluedPopulation op t -> m (SelectionPopulation op t))
    , _fitness :: GProgram op t -> m Double
    }
makeLenses ''GPEvolutionSetup

newtype EvolutionState = EvolutionState
    { _generation :: Int
    }

startEvolution :: (Monad m) => Evolution m ()
startEvolution = Evolution $ put $ EvolutionState 0

newtype Evolution m a = Evolution { runEvolution :: StateT EvolutionState m a }
    deriving (Functor, Applicative, Monad, MonadTrans)

evolve
    :: (Monad m)
    => GPEvolutionSetup m op t
    -> Evolution m ()
evolve setup = do
    initialPopulation <- mkInitialPopulation
    evolveWithInitPopulation setup initialPopulation
  where
    mkInitialPopulation = mkInitial <$> lift (V.replicateM
        (setup ^. populationSize)
        (setup ^. initMethod))

evolveWithInitPopulation
    :: (Monad m)
    => GPEvolutionSetup m op t
    -> InitialPopulation op t
    -> Evolution m ()
evolveWithInitPopulation setup population = do
    startEvolution
    evolveWith setup (mkGenerationZero population)

evolveWith
    :: (Monad m)
    => GPEvolutionSetup m op t
    -> Generation op t
    -> Evolution m ()
evolveWith setup population = do

