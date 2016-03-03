{-# LANGUAGE NoImplicitPrelude #-}

module Main (main)
  where

import System.IO (IO)

import Test.Framework (defaultMain)

import TestCase (tests)


main :: IO ()
main = defaultMain tests
