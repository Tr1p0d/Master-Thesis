{-# LANGUAGE NoImplicitPrelude #-}

module TestCase.AI.GP.Mutation where

import Control.Monad.Identity (Identity(Identity, runIdentity))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (head)
import Data.Maybe (Maybe(Just, Nothing))
import Text.Show (Show)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import AI.GP.Type.GProgram (GProgram(Node, Leaf))
import AI.GP.Mutation (pointMutationGen)


data Operation
    = Plus
    | Minus
    deriving (Eq, Show)

data Terminal
    = Const Int
    deriving (Eq, Show)
tests :: [Test]
tests =
    [ testGroup "Point mutation"
        [ testCase "Point mutation non-empty CR" test_PointMutation_NonEmpty_CR
        , testCase "Point mutation empty CR" test_PointMutation_Empty_CR
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
        (Node 1 Plus
            (Leaf 0 (Const 1))
            (Leaf 0 (Const 1))
        )
        (Node 1 Plus
            (Leaf 0 (Const 1))
            (Leaf 0 (Const 1))
        )

test_PointMutation_NonEmpty_CR :: Assertion
test_PointMutation_NonEmpty_CR = testTree2 @=? actual
  where
    actual = runIdentity $ pointMutationGen
        testTree1
        (Identity 2)
        (Identity . head)
        (Identity Minus)

test_PointMutation_Empty_CR :: Assertion
test_PointMutation_Empty_CR = testTree1 @=? actual
  where
    actual = runIdentity $ pointMutationGen
        testTree1
        (Identity 3)
        (Identity . head)
        (Identity Minus)
