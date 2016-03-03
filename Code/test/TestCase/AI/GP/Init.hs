{-# LANGUAGE NoImplicitPrelude #-}

module TestCase.AI.GP.Init where

import Control.Monad.Identity (Identity(Identity, runIdentity))
import Data.Eq (Eq)
import Data.Int (Int)
import Text.Show (Show)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@?=))

import AI.GP.Type.GProgram (GProgram(Node, Leaf))
import AI.GP.Init (full)


tests :: [Test]
tests =
    [ testGroup "Full init method"
        [ testCase "full non-negative height" test_FullMethod_NonNegativeHeight
        , testCase "full zero height" test_FullMethod_ZeroHeight
        --, testCase "full negative height" test_FullMethod_NegativeHeight
        ]
    ]

data Operation
    = Plus
    | Minus
    deriving (Eq, Show)

data Terminal
    = Const Int
    deriving (Eq, Show)

test_FullMethod_NonNegativeHeight :: Assertion
test_FullMethod_NonNegativeHeight = expected @?= actual
  where
    actual = runIdentity (full (Identity Plus) (Identity (Const 1)) 2)
    expected =
        Node 2 Plus
            (Node 1 Plus
                (Leaf 0 (Const 1))
                (Leaf 0 (Const 1))
            )
            (Node 1 Plus
                (Leaf 0 (Const 1))
                (Leaf 0 (Const 1))
            )

test_FullMethod_ZeroHeight :: Assertion
test_FullMethod_ZeroHeight = expected @?= actual
  where
    actual = runIdentity (full (Identity Plus) (Identity (Const 1)) 0)
    expected = Leaf 0 (Const 1)

test_FullMethod_NegativeHeight :: Assertion
test_FullMethod_NegativeHeight = expected @?= actual
  where
    actual = runIdentity (full (Identity Plus) (Identity (Const 1)) (-1))
    expected = Leaf 0 (Const 1)
