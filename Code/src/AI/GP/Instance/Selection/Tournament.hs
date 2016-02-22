{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Instance.Selection.Tournament where

import Prelude (Double)

import Data.Foldable (asum)
import Data.Function ((.))
import Data.Int (Int)
import Data.Traversable (sequence)
import Control.Applicative ((<$>))

import Control.Lens ((^.))
import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.SelectionMethod (SelectionMethod, select)
import AI.GP.Class.Population (Population, replicate, sample)
import AI.GP.Class.Program (Program)
import AI.GP.Type.Class.Selection.Tournament
    (Tournament
    , rounds
    , roundSize
    )
import AI.GP.Type.Fitnesse (Fitness, discardFitness)
import AI.GP.Type.Population (getEvalutedPopulation, mkSelection)

instance SelectionMethod Tournament where
    select tournament evaluatedPopulation = mkSelection <$> tournamentSelection
        (tournament ^. roundSize)
        (tournament ^. rounds)
        (evaluatedPopulation ^. getEvalutedPopulation)

tournamentSelection
    :: (MonadRandom m, Population p, Program e)
    => Int -- | Round size
    -> Int -- | Rounds in tournament
    -> p (Fitness e)
    -> m (p e)
tournamentSelection roundSize rounds =
    tournamentRound roundSize

tournamentRound
    :: (MonadRandom m, Population p, Program e)
    => Int -- | Round size
    -> p (Fitness e)
    -> m (p e)
tournamentRound roundSize ep =
    replicate roundSize (discardFitness <$> sample ep)
