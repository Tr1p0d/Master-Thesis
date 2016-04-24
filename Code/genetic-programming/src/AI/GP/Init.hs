{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Init where

import Prelude ((+))

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (Monad(return))
import Data.Bool (Bool)
import Data.Int (Int)
import Data.Function (($))
import Data.List (length)
import Data.Ord ((>), max)
import GHC.Num ((-))

import Control.Monad.Primitive (PrimMonad, PrimState)

import System.Random.MWC (Gen, uniformR)

import AI.GP.Type.GProgram (GProgram(Leaf, Node), Individual, _height)
import AI.GP.Utils (arbitraryUniform)


fullInit
    :: (PrimMonad m)
    => [op]
    -> [t]
    -> Int
    -> Gen (PrimState m)
    -> m (Individual op t)
fullInit ops terms height token = fullInitGen opSelector termSelector height
  where
    opSelector = arbitraryUniform token ops
    termSelector = arbitraryUniform token terms

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

growInit
    :: (PrimMonad m)
    => [op]
    -> [t]
    -> Int
    -> Gen (PrimState m)
    -> m (Individual op t)
growInit ops terminals height token =
    growInitGen opSelector termSelector height termGen
  where
    opSelector = arbitraryUniform token ops
    termSelector = arbitraryUniform token terminals
    termGen = do
        num <- uniformR (0, length ops + length terminals) token
        return $ length terminals > num

growInitGen
    :: (Monad m)
    => m op
    -> m t
    -> Int
    -> m Bool
    -> m (Individual op t)
growInitGen _ tG 0 _ = Leaf 0 <$> tG
growInitGen opG tG n genT = do
    op <- opG
    lst <- possiblySubProg
    rst <- possiblySubProg
    -- This thing needs to be fixed so it generates shorter than 1
    return $ Node (max (_height lst) (_height rst) + 1) op lst rst
  where
    possiblySubProg = do
        leaf <- genT
        if leaf then Leaf 0 <$> tG else growInitGen opG tG (n-1) genT

