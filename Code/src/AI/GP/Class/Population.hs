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

import Data.Traversable (Traversable)

--import AI.GP.Class.GPExpression (GPExpression)

class (Traversable p) => Population p where
