{-# LANGUAGE NoImplicitPrelude #-}

module TestCase.AI.GP.Crossover where

import Control.Monad.Identity (Identity(Identity, runIdentity))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (head)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Show (Show)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import AI.GP.Type.GProgram (GProgram(Node, Leaf))
import AI.GP.Crossover (subtreeCrossoverGen)


data Operation
    = Plus
    | Minus
    deriving (Eq, Show)

data Terminal
    = Const Int
    deriving (Eq, Show)

tests :: [Test]
tests =
    [ testGroup "Subtree crossover"
        [ testCase "Subtree crossover non-empty CR"
            test_SubteeCrossover_NonEmpty_CR
        , testCase "Subtree crossover empty CR"
            test_SubteeCrossover_Empty_CR
        ]
    ]

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

test_SubteeCrossover_NonEmpty_CR :: Assertion
test_SubteeCrossover_NonEmpty_CR = Just (testTree2, testTree1) @?= actual
  where
    actual = runIdentity $ subtreeCrossoverGen
        (testTree1, testTree2) (Identity 2) (Identity . head)


test_SubteeCrossover_Empty_CR :: Assertion
test_SubteeCrossover_Empty_CR = Nothing @?= actual
  where
    actual = runIdentity $ subtreeCrossoverGen
        (Leaf 0 (Const 1), testTree2) (Identity 2) (Identity . head)
