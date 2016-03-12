module Main where

import Control.Monad.State.Lazy (runStateT)

import Data.Random (MonadRandom)

import AI.GP
    ( Evolution
    , EvolutionResult
    , GPEvolutionParams(GPEvolutionParams)
    , evalEvolution
    , evolve
    , getArbitraryIndividual
    , runEvolution
    )
import AI.GP.Selection (tournamentSelection)
import AI.GP.Init (fullInit)
import AI.GP.Crossover (subtreeCrossoverPreferLeafs)
import AI.GP.Mutation (pointMutationPreferLeafs)
import AI.GP.Type.Fitnesse (discardFitness)
import AI.GP.Type.GProgram


data Operators
    = Plus
    | Times
    | Minus
    deriving (Show, Eq)

data Terminals
    = X
    deriving (Show, Eq)

eval val (Leaf _ X) = val
eval val (Node _ Plus l r) = eval val l + eval val r
eval val (Node _ Times l r) = eval val l * eval val r
eval val (Node _ Minus l r) = eval val l - eval val r

fitness :: Monad m => GProgram Operators Terminals -> m Double
fitness program = return $
    diff (eval 1 program) 1
    + diff (eval 2 program) 4
    + diff (eval 3 program) 9
  where
    diff a b = abs (a - b)

main :: IO ()
main = evalEvolution linearRegression >>= print

linearRegressionParams
    :: (MonadRandom m)
    => GPEvolutionParams m Operators Terminals
linearRegressionParams = GPEvolutionParams
    100   -- ^ use 100 programs
    (fullInit [Plus, Times, Minus] [X] 2)  -- ^ use full method
    (tournamentSelection 5 5) -- ^ use tournament selection
    fitness
    (< 1.0)
    5 -- ^ breed 50 offsprings
    (subtreeCrossoverPreferLeafs 2 0.7)
    0.2
    (pointMutationPreferLeafs [Plus, Times ,Minus] 0.1)
    (\p -> discardFitness <$> getArbitraryIndividual p)

linearRegression
    :: (MonadRandom m)
    => Evolution Operators Terminals m (EvolutionResult Operators Terminals)
linearRegression = evolve linearRegressionParams
