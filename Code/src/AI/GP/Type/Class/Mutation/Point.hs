{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Class.Mutation.Point where

import Prelude (Double)

import Data.Int(Int)

import Control.Lens (makeLenses)

data MutationDistribution
    = Uniform
    | SizeIncreasing
    | NegativeSizeIncreasing

data Point = Point
    { _distribution :: MutationDistribution
    , _mutationProbability :: Double
    , _depth :: Int
    }
makeLenses ''Point

