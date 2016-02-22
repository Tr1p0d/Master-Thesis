{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  The Population type class
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  AllRightsReserved (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The population is represented by a type class so we can parameterize
-- over various containers for performance purposes.
-------------------------------------------------------------------------------
module AI.GP.Class.Population where

import Control.Applicative ((<$>))
import Control.Monad (Monad)
import qualified Control.Monad as L (replicateM)
import Control.Monad.Random (getRandomR)
import Control.Monad.Random.Class (MonadRandom)

import qualified Data.List as L ((!!))
import Data.Foldable (length)
import Data.Functor (Functor)
import Data.Int (Int)
import Data.Traversable (Traversable)

import qualified Data.Vector as V (Vector, (!), replicateM)

class (Functor p, Traversable p) => Population p where
    replicate :: (Monad m) => Int -> m a -> m (p a)
    sample :: (MonadRandom m) => p a -> m a

instance Population [] where
    sample list = (list L.!!) <$> getRandomR (0, length list)
    replicate = L.replicateM

instance Population V.Vector where
    sample v = (v V.!) <$> getRandomR (0, length v)
    replicate = V.replicateM
