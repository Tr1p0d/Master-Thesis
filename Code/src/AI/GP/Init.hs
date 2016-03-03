{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Init where

import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Function (($))
import Data.Int (Int)
import GHC.Num ((-))

import AI.GP.Type.GProgram (GProgram(Leaf, Node))

full
    :: (Applicative m)
    => m op
    -> m t
    -> Int
    -> m (GProgram op t)
full opG tG = \case
    0 -> Leaf 0 <$> tG
    n -> Node n <$> opG <*> subProg n <*> subProg n
  where
    subProg n = full opG tG $ (n - 1)
