{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module AI.GP where

import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>>=), join)
import Control.Monad.Random (getRandom, getRandomR)
import Control.Monad.Random.Class (MonadRandom)
import Data.Bool (bool)
import Data.Function (($), (.))
import Data.List ((!!), (++), length)
import Prelude (Int, (-), (+), (<), div)

import AI.GP.Class.GPExpression (GPExpression, terminalSet, nonterminalSet)

sample :: MonadRandom m => [a] -> m a
sample list = (list !!) <$> getRandomR (0, length list)

fullMethod :: (GPExpression m e) => Int -> m e
fullMethod 0 = join $ sample terminalSet
fullMethod n = join (sample nonterminalSet) <*> subExpr <*> subExpr
  where
    subExpr = fullMethod $ n - 1

--growMethod :: (GPExpression m e) => Int -> m e
--growMethod 0 = join $ sample terminalSet
--growMethod n = do
--    r <- getRandomR (0, length terminalSet + length nonterminalSet)
--    if r < 3 then (join $ sample terminalSet) else nonterminal'
--  where
--    nonterminal' = join (sample nonterminalSet) <*> subExpr <*> subExpr
--    subExpr = growMethod $ n - 1
