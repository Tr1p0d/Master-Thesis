{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Class.Program where

import Prelude (Double)

import Control.Monad.Random.Class (MonadRandom)

type RandomNonterminal m e = m (e -> e -> e)
type RandomTerminal m e = m e

class Program e where
    rT :: MonadRandom m => RandomTerminal m e
    rNT :: MonadRandom m => RandomNonterminal m e
    eval :: e -> Double

