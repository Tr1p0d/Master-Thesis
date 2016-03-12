{-# LANGUAGE NoImplicitPrelude #-}

module TestCase.AI.GP.Type.Population
    (tests)
  where

import Prelude (fromIntegral)

import Control.Monad (return)
import Data.Bool (Bool(False))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (head)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Show (Show)

import Control.Monad.Identity (Identity(Identity, runIdentity))
import qualified Data.Vector as V (fromList, head, replicate)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import AI.GP.Type.Fitnesse (Fitness(Fitness), _getIndividual)
import AI.GP.Type.GProgram (GProgram(Node, Leaf))
import AI.GP.Type.Population
    ( crossPopulation
    , _getPopulation
    , evalPopulation
    , mkBreed
    , mkGeneration
    , mkMuted
    , mkEvalued
    , mkSelection
    , mutePopulation
    , populationLength
    , replenishPopulation
    )


tests :: [Test]
tests = [ testGroup "Population Mutation"
            [ testCase "Population size invariant against mutation #1"
                test_MutePopulation_Size_1
            , testCase "Population size invariant against mutation #2"
                test_MutePopulation_Size_2
            ]
        , testGroup "Population Crossover"
            [ testCase "Crossover even population size."
                test_CrossPopulation_Size_even
            , testCase "Crossover odd population size."
                test_CrossPopulation_Size_odd
            ]
        , testGroup "Replenish Population"
            [ testCase "Population size invariant against replenish #1"
                test_ReplenishPopulation_Size_1
            , testCase "Population size invariant against replenish #2"
                test_ReplenishPopulation_Size_2
            ]
        , testGroup "Various utility functions."
            [ testCase "Eval population"
                test_EvalPopulation
            ]
        ]

data Operation
    = Plus
    | Minus
    deriving (Eq, Show)

data Terminal
    = Const Int
    deriving (Eq, Show)

testTree1 =
    Node 2 Plus
        (Node 1 Plus
            (Leaf 0 (Const 1))
            (Leaf 0 (Const 1))
        )
        (Node 1 Plus
            (Leaf 0 (Const 1))
            (Leaf 0 (Const 1))
        )

testTree2 =
    Node 2 Minus
        (Node 1 Minus
            (Leaf 0 (Const 2))
            (Leaf 0 (Const 2))
        )
        (Node 1 Minus
            (Leaf 0 (Const 2))
            (Leaf 0 (Const 2))
        )

testEvalTree1 = Fitness
    testTree1
    1.0
    False

testLeaf1 = Leaf 0 (Const 1)
testLeaf2 = Leaf 0 (Const 2)

test_EvalPopulation :: Assertion
test_EvalPopulation = expected @=? actual
  where
    actual = runIdentity $ evalPopulation
        (\i -> return $ Fitness i (fromIntegral $ fitness i) False)
        (mkGeneration $ V.fromList [testLeaf1, testLeaf2])
    expected = mkEvalued $ V.fromList
        [ Fitness testLeaf1 1 False
        , Fitness testLeaf2 2 False
        ]
    fitness (Leaf _ (Const i)) = i
    fitness (Node _ Plus _ _) = let a=a in a

test_MutePopulation_Size_1 :: Assertion
test_MutePopulation_Size_1 = do
    muted <- mutePopulation breed 0.1 return
    expectedLength @=? populationLength muted
  where
    expectedLength = 2
    breed = mkBreed $ V.fromList [testTree1, testTree2]

test_MutePopulation_Size_2 :: Assertion
test_MutePopulation_Size_2 = do
    muted <- mutePopulation breed 0.9 return
    expectedLength @=? populationLength muted
  where
    expectedLength = 10000
    breed = mkBreed $ V.replicate 10000 testTree1

test_CrossPopulation_Size_even :: Assertion
test_CrossPopulation_Size_even = do
    cross <- crossPopulation selection expectedLength return
    expectedLength @=? populationLength cross
  where
    expectedLength = 10
    selection = mkSelection $ V.replicate 20 testTree2

test_CrossPopulation_Size_odd :: Assertion
test_CrossPopulation_Size_odd = do
    cross <- crossPopulation selection expectedLength return
    expectedLength @=? populationLength cross
  where
    expectedLength = 9
    selection = mkSelection $ V.replicate 20 testTree2

test_ReplenishPopulation_Size_1 :: Assertion
test_ReplenishPopulation_Size_1 = do
    gen <- replenishPopulation
        (return . _getIndividual . V.head . _getPopulation)
        evalued muted
    populationLength evalued @=? populationLength gen
  where
    evalued = mkEvalued $ V.replicate 20 testEvalTree1
    muted = mkMuted $ V.replicate 20 testTree1

test_ReplenishPopulation_Size_2 :: Assertion
test_ReplenishPopulation_Size_2 = do
    gen <- replenishPopulation
        (return . _getIndividual . V.head . _getPopulation)
        evalued muted
    populationLength evalued @=? populationLength gen
  where
    evalued = mkEvalued $ V.replicate 10 testEvalTree1
    muted = mkMuted $ V.replicate 1 testTree1
