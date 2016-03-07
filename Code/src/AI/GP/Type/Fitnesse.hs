{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Fitnesse where

import Prelude (Double)

import Control.Monad (Monad(return))
import Data.Bool (Bool(False), otherwise)
import Data.Eq ((==), Eq)
import Data.Function (($))
import Data.Ord ((>), (<), Ord, Ordering(LT, EQ, GT), compare)

import Control.Lens ((^.), makeLenses)

import AI.GP.Type.GProgram (GProgram)

data Fitness e = Fitness
    { _getIndividual :: e
    , _getScore :: Double
    , _fitEnough :: Bool
    }
makeLenses ''Fitness

instance Eq (Fitness a) where
    a == b = (a ^. getScore) == (b ^. getScore)

instance Ord (Fitness a) where
    compare a b
       | (a ^. getScore) > (b ^. getScore) = GT
       | (a ^. getScore) < (b ^. getScore) = LT
       | otherwise = EQ

evalIndividual
    :: (Monad m)
    => (GProgram op t -> m Double)
    -> (Double -> Bool)
    -> GProgram op t
    -> m (Fitness (GProgram op t))
evalIndividual eval cond individual = do
    fitness <- eval individual
    return $ mkFitness individual fitness (cond fitness)

emptyFitness :: GProgram op t -> Fitness (GProgram op t)
emptyFitness p = Fitness p 0.0 False

discardFitness :: Fitness e -> e
discardFitness =  _getIndividual

mkFitness :: e -> Double -> Bool -> Fitness e
mkFitness = Fitness
