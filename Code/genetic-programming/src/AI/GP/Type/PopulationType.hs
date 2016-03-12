{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Type.PopulationType where

import Data.Eq (Eq)
import Text.Show (Show)


data PopulationType
    = Breed
    | Evaluated
    | Fittest
    | Generation
    | Initial
    | Muted
    | Selection
    deriving (Eq, Show)
