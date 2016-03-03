{-# LANGUAGE NoImplicitPrelude #-}

module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.AI.GP.Init as Init (tests)


tests :: [Test]
tests = [ testGroup "TestCase.AI.GP.Init" Init.tests ]
