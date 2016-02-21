{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Type.PopulationType where

data PopulationType
    = Breed
    | Fittest
    | Generation
    | Initial
    | Muted
    | Selection

