{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Init where

import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Int (Int)
import GHC.Num ((-))

import Data.Random (MonadRandom)

import AI.GP.Type.GProgram (GProgram(Leaf, Node))
import AI.GP.Utils (arbitraryUniform)


fullInit
    :: (MonadRandom m)
    => [op]
    -> [t]
    -> Int
    -> m (GProgram op t)
fullInit ops terms = fullInitGen opSelector termSelector
  where
    opSelector = arbitraryUniform ops
    termSelector = arbitraryUniform terms

fullInitGen
    :: (Applicative m)
    => m op
    -> m t
    -> Int
    -> m (GProgram op t)
fullInitGen opG tG = \case
    0 -> Leaf 0 <$> tG
    n -> Node n <$> opG <*> subProg n <*> subProg n
  where
    subProg n = fullInitGen opG tG (n - 1)
