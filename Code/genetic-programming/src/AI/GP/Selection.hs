{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Selection where

import Control.Applicative ((<$>))
import Control.Monad (Monad)
--import Data.Bool (otherwise)
import Data.Function ((.))
import Data.Int (Int)

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V (Vector, maximum, replicateM)

import System.Random.MWC (Gen)

import AI.GP.Type.Fitnesse (EvaluatedIndividual, discardFitness)
import AI.GP.Type.GProgram (Individual)
import AI.GP.Utils (arbitraryUniformVector)


tournamentSelection
    :: (PrimMonad m)
    => Int -- ^ Tournament size
    -> Int -- ^ Tournament rounds
    -> Gen (PrimState m)
    -> V.Vector (EvaluatedIndividual op t)
    -> m (V.Vector (Individual op t))
tournamentSelection size rounds token evaluatedPop =
    tournamentGen size rounds selector evaluatedPop
  where
    selector pop = arbitraryUniformVector token pop

tournamentGen
    :: (Monad m)
    => Int -- ^ Tournament size
    -> Int -- ^ Tournament rounds
    -> (V.Vector (EvaluatedIndividual op t) -> m (EvaluatedIndividual op t))
    -> V.Vector (EvaluatedIndividual op t)
    -> m (V.Vector (Individual op t))
tournamentGen size rounds selector population =
    V.replicateM rounds (tournamentRound size selector population)

tournamentRound
    :: (Monad m)
    => Int
    -> (V.Vector (EvaluatedIndividual op t) -> m (EvaluatedIndividual op t))
    -> V.Vector (EvaluatedIndividual op t)
    -> m (Individual op t)
tournamentRound n select evaluated =
    -- this could be much faster when sorted
    discardFitness . V.maximum <$> V.replicateM n (select evaluated)
--
--tournamentRound
--    :: (Monad m)
--    => Int
--    -> (EvaluedPopulation op t -> m (GProgram op t))
--    ->m (SelectionPopulation op t)
--tournamentRound n selector
--    | n > 0 = let a=a in a
--    | otherwise = return emptySelection

