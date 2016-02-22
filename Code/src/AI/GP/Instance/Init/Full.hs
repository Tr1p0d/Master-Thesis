{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Instance.Init.Full where

import Prelude ((-))

import Control.Applicative ((<$>), (<*>))
import Data.Function (($))
import Data.Int (Int)

import Control.Lens ((^.))
import Control.Monad.Random.Class (MonadRandom)

import AI.GP.Class.InitMethod (InitMethod, init)
import AI.GP.Class.Population (replicate)
import AI.GP.Class.Program (Program, rT, rNT)
import AI.GP.Type.Class.Init.Full (Full, depth, populationSize)
import AI.GP.Type.Population (mkInitial)

instance InitMethod Full where
    init program = mkInitial <$> rPopulation rProgram
      where
        rProgram = initMethodFull (program ^. depth)
        rPopulation rp = replicate (program ^. populationSize) rp

initMethodFull :: (MonadRandom m, Program e) => Int -> m e
initMethodFull 0 = rT
initMethodFull n = rNT <*> subExpr <*> subExpr
  where
    subExpr = initMethodFull $ n - 1
