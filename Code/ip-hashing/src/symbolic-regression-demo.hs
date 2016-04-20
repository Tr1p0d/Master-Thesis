-- |
-- Module      : Main
-- Description : The symbolic approximation demo
-- Copyright   : (c) Marek Kidon, 2016
-- License     : BSD
-- Maintainer  : xkidon00@stud.fit.vutbr.cz
-- Stability   : experimental
-- Portability :
--
-- This module contains specification for GP evolution run
-- to approximate the @x^2 + x@ function.
--
module Main where

import Control.Monad ((>=>))

import Control.Monad.Primitive (PrimMonad)

import qualified Data.Vector as V (mapM)

import Control.Lens ((^.))
import System.Random.MWC (withSystemRandom)

import AI.GP
    ( Evolution
    , GPEvolutionParams(GPEvolutionParams)
    , evalEvolution
    , evolve
    )
import AI.GP.Type.Evolution (individual)
import AI.GP.Type.Fitnesse (individualScore)
import AI.GP.Selection (tournamentSelection)
import AI.GP.Init (fullInit, growInit)
import AI.GP.Crossover (subtreeCrossoverUniform)
import AI.GP.Mutation (pointMutationPreferLeafs, subtreeMutationUniform)
import AI.GP.Type.Fitnesse (discardFitness)
import AI.GP.Type.GProgram


-- |Operators used
data Operators
    = Plus
    | Times
    | Minus
    deriving (Show, Eq)

-- |Terminals used
data Terminals
    = X
    deriving (Show, Eq)

-- |The terminal and operators evaluation
eval :: Double -> GProgram Operators Terminals -> Double
eval val (Leaf _ X) = val
eval val (Node _ Plus l r) = eval val l + eval val r
eval val (Node _ Times l r) = eval val l * eval val r
eval val (Node _ Minus l r) = eval val l - eval val r

-- |The fitness function
fitness :: Monad m => GProgram Operators Terminals -> m Double
fitness program = return $
    diff (eval 1 program) 2
    + diff (eval 2 program) 6
    + diff (eval 3 program) 12
  where
    diff a b = abs (a - b)

-- |Run the evolution and collect the results
main :: IO ()
main = withSystemRandom $ evalEvolution linearRegression
    >=> print . map (^. individual . individualScore)

-- |Linear regression evolution parameters
linearRegressionParams
    :: (PrimMonad m)
    => GPEvolutionParams m Operators Terminals
linearRegressionParams = GPEvolutionParams
    25
    (growInit [Plus, Times, Minus] [X] 2)
    (tournamentSelection 2 20) -- ^ use tournament selection
    fitness
    (< 1.0)
    24
    (subtreeCrossoverUniform)
    0.5
    (subtreeMutationUniform [Plus, Times ,Minus] [X])
    (\_ p -> V.mapM (return . discardFitness) p)
    30

-- |Evolution
linearRegression
    :: (PrimMonad m)
    => Evolution Operators Terminals m ()
linearRegression = evolve linearRegressionParams
