{-# LANGUAGE NoImplicitPrelude #-}

module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.AI.GP.Init as Init (tests)
import qualified TestCase.AI.GP.Crossover as Crossover (tests)


tests :: [Test]
tests =
    [ testGroup "TestCase.AI.GP.Init" Init.tests
    , testGroup "TestCase.AI.GP.Crossver" Crossover.tests
    ]
