{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Selection where

--import Prelude ((>))

import Control.Applicative ((<$>))
import Control.Monad (Monad)
--import Data.Bool (otherwise)
import Data.Function ((.))
import Data.Int (Int)

import qualified Data.Vector as V (maximum, replicateM)

import AI.GP.Type.Fitnesse (Fitness, discardFitness)
import AI.GP.Type.GProgram (GProgram)
import AI.GP.Type.Population
    ( EvaluedPopulation
    , SelectionPopulation
--    , emptySelection
    , mkSelection
    )

tournamentGen
    :: (Monad m)
    => Int -- ^ Tournament size
    -> Int -- ^ Tournament rounds
    -> (EvaluedPopulation op t -> m (Fitness (GProgram op t))) -- ^ selector
    -> EvaluedPopulation op t
    -> m (SelectionPopulation op t)
tournamentGen size rounds selector population =
    mkSelection <$> V.replicateM rounds (tournamentRound size selector population)

tournamentRound
    :: (Monad m)
    => Int
    -> (EvaluedPopulation op t -> m (Fitness (GProgram op t))) -- ^ selector
    -> EvaluedPopulation op t
    -> m (GProgram op t)
tournamentRound n select evaluated =
    discardFitness . V.maximum <$> V.replicateM n (select evaluated)

--tournamentRound
--    :: (Monad m)
--    => Int
--    -> (EvaluedPopulation op t -> m (GProgram op t))
--    ->m (SelectionPopulation op t)
--tournamentRound n selector
--    | n > 0 = let a=a in a
--    | otherwise = return emptySelection
