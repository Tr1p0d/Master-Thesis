{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
module AI.GP.Class.GPExpression where

import Control.Monad.Random.Class (MonadRandom)

type TerminalSet m e = [m e]
type NonTerminalSet m e = [m (e -> e -> e)]

class MonadRandom m => GPExpression m e where
    terminalSet :: TerminalSet m e
    nonterminalSet :: NonTerminalSet m e
