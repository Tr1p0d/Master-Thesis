{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Utils where

import Control.Applicative ((<$>))
import Data.Foldable (length)
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((!!))
import GHC.Num ((-))

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V (Vector, (!), cons, empty, foldl)

import System.Random.MWC (Gen, uniformR)

arbitrary :: (Functor m) => m Int -> [a] -> m a
arbitrary selector list = (list !!) <$> selector

arbitraryUniform :: (PrimMonad m) => Gen (PrimState m) -> [a] -> m a
arbitraryUniform token list = arbitrary selector list
  where
    selector = uniformR (0, (length list - 1)) token

arbitraryVector :: (Functor m) => m Int -> V.Vector a -> m a
arbitraryVector selector list = (list V.!) <$> selector

arbitraryUniformVector
    :: (PrimMonad m)
    => Gen (PrimState m)
    -> V.Vector a
    -> m a
arbitraryUniformVector token vec = arbitraryVector selector vec
  where
    selector = uniformR (0, (length vec) - 1) token

flatten :: V.Vector (a,a) -> V.Vector a
flatten = V.foldl (\acc (a, b) -> a `V.cons` (b `V.cons` acc)) V.empty
