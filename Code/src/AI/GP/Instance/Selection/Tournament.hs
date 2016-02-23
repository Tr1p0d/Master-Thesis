{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Instance.Selection.Tournament where

import Data.Int (Int)
import Control.Applicative ((<$>))

import Control.Lens ((^.))
import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.SelectionMethod (SelectionMethod, select)
import AI.GP.Class.Population (Population, replicate, merge, sample)
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
tournamentSelection rs rnds ep =
    merge <$> replicate rnds (tournamentRound rs ep)

tournamentRound
    :: (MonadRandom m, Population p, Program e)
    => Int -- | Round size
    -> p (Fitness e)
    -> m (p e)
tournamentRound rs ep = replicate rs (discardFitness <$> sample ep)
