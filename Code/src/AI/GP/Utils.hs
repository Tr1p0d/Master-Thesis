{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Utils where

import Control.Applicative ((<$>))
import Data.Foldable (length)
import Data.Function (($))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((!!))
import GHC.Num ((-))

import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Uniform (uniform)


arbitrary :: (Functor m) => m Int -> [a] -> m a
arbitrary selector list = (list !!) <$> selector

arbitraryUniform :: (MonadRandom m) => [a] -> m a
arbitraryUniform list = arbitrary selector list
  where
    selector = sample $ uniform 0 ((length list) -1)
