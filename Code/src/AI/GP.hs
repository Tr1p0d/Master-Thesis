{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP where

--import Control.Applicative (Applicative, (<$>))
--import Control.Monad (Monad((>>=)))
--import Data.Foldable (any)
--import Data.Function (($), (.))
--import Data.Functor (Functor(fmap))

import Data.Int (Int)
import Data.Maybe (Maybe)

import AI.GP.Type.GProgram (GProgram)

data GPEvolution m op t = GPEvolution
    { _populationSize :: Int
    , _initMethod :: m (GProgram op t)
    , _mutationMethod :: m (Maybe (GProgram op t))
    , _crossoverMethod :: m (Maybe (GProgram op t, GProgram op t))
    --, _selectionMethod ::
    }
