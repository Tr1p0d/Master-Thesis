{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Utils where

import Control.Applicative ((<$>))
import Data.Foldable (length)
import Data.Function (($))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((!!))
import GHC.Num ((-))

import qualified Data.Vector as V (Vector, (!), cons, empty, foldl)

import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Uniform (uniform)


arbitrary :: (Functor m) => m Int -> [a] -> m a
arbitrary selector list = (list !!) <$> selector

arbitraryUniform :: (MonadRandom m) => [a] -> m a
arbitraryUniform list = arbitrary selector list
  where
    selector = sample $ uniform 0 (length list - 1)

arbitraryVector :: (Functor m) => m Int -> V.Vector a -> m a
arbitraryVector selector list = (list V.!) <$> selector

arbitraryUniformVector :: (MonadRandom m) => V.Vector a -> m a
arbitraryUniformVector vec = arbitraryVector selector vec
  where
    selector = sample $ uniform 0 (length vec - 1)

flatten :: V.Vector (a,a) -> V.Vector a
flatten = V.foldl (\acc (a, b) -> a `V.cons` (b `V.cons` acc)) V.empty
