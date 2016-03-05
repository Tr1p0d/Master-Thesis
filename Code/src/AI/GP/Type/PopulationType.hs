{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Type.PopulationType where

data PopulationType
    = Breed
    | Evaluated
    | Fittest
    | Generation
    | Initial
    | Muted
    | Selection

