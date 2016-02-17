{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude (Float)

import Control.Monad (replicateM, return)
import Control.Monad.Random (Rand, StdGen, getRandomR, evalRandIO)
import Data.Function (($), const)
import Data.Int (Int)
import Data.Proxy (Proxy)
import qualified Data.Vector as V (Vector, empty, fromList, length)
import System.IO (IO)
import Text.Show (Show)

import AI.GP (fullMethod)
import AI.GP.Class.GPExpression (GPExpression, terminalSet, nonterminalSet)
import AI.GP.Class.Population as P (Population)

main ::  IO ()
main = let a=a in a

data E
    = Plus E E
    | Const Int
    deriving (Show)

instance GPExpression (Rand StdGen) E where

instance Population V.Vector where

class GPRun m a where
    type PopulationType a :: * -> *

    type IndividualType a :: *

    initPopulation
        ::
        ( p ~ PopulationType a
        , i ~ IndividualType a
        , Population p
        , GPExpression m i
        )
        => Proxy a -> m (p i)

    populationSize :: Proxy a -> Int

    type FitnesseType a :: k

    fitness :: Proxy a -> FitnesseType Sample

data Sample
data Sample2

instance GPRun (Rand StdGen) Sample where
    type PopulationType Sample = V.Vector
    type IndividualType Sample = E

    populationSize = const 10
    initPopulation = const $ return V.empty

    type FitnesseType Sample = (IndividualType Sample) -> V.Vector Int -> Float

instance GPRun (Rand StdGen) Sample2 where
    type FitnesseType Sample2 = (IndividualType Sample2) -> Float
