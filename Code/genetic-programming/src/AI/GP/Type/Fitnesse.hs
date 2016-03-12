{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Fitnesse where

import Prelude (Double)

import Control.Monad (Monad(return))
import Data.Bool (Bool(False), otherwise)
import Data.Eq ((==), Eq)
import Data.Function (($))
import Data.Ord ((>), (<), Ord, Ordering(LT, EQ, GT), compare)
import Text.Show (Show)

import Control.Lens ((^.), makeLenses)
import Data.Default (Default(def))

import AI.GP.Type.GProgram (Individual, GProgram)


data Fitness e = Fitness
    { _getIndividual :: e
    , _getScore :: Double
    , _fitEnough :: Bool
    }
    deriving (Show)
makeLenses ''Fitness

instance Eq (Fitness a) where
    a == b = (a ^. getScore) == (b ^. getScore)

instance Ord (Fitness a) where
    compare a b
       | (a ^. getScore) > (b ^. getScore) = GT
       | (a ^. getScore) < (b ^. getScore) = LT
       | otherwise = EQ

instance (Default a) => Default (Fitness a) where
    def = Fitness def 0.0 False

type EvaluatedIndividual op t = Fitness (GProgram op t)

evalIndividual
    :: (Monad m)
    => (Individual op t -> m Double)
    -> (Double -> Bool)
    -> Individual op t
    -> m (EvaluatedIndividual op t)
evalIndividual eval cond individual = do
    fitness <- eval individual
    return $ mkFitness individual fitness (cond fitness)

emptyFitness :: Individual op t -> EvaluatedIndividual op t
emptyFitness p = Fitness p 0.0 False

discardFitness :: Fitness e -> e
discardFitness =  _getIndividual

mkFitness :: e -> Double -> Bool -> Fitness e
mkFitness = Fitness
