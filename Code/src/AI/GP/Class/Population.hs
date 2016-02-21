{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  The Population type class
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The population is represented by a type class so we can parameterize
-- over various containers for performance purposes.
-------------------------------------------------------------------------------
module AI.GP.Class.Population where

import Control.Applicative ((<$>))
import Control.Monad.Random (getRandomR)
import Control.Monad.Random.Class (MonadRandom)

import Data.List ((!!))
import Data.Foldable (length)
import Data.Traversable (Traversable)
import qualified Data.Vector as V (Vector, length, (!))

class (Traversable p) => Population p where
    sample :: (MonadRandom m) => p a -> m a

instance Population [] where
    sample list = (list !!) <$> getRandomR (0, length list)

instance Population V.Vector where
    sample v = (v V.!) <$> getRandomR (0, V.length v)
