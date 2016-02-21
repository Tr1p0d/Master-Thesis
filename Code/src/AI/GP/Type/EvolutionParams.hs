{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.EvolutionParams where

import Prelude (Double)
import Data.Bool (Bool)
import Control.Lens (makeLenses)

data EvolutionParams i s b mut el (p :: * -> *) e = EvolutionParams
    { _breedMethod :: b
    , _ellitismMethod :: el
    , _mutationMethod :: mut
    , _selectionMethod  :: s
    , _terminationCondition :: Double -> Bool
    , _initMethod :: i
    }
makeLenses ''EvolutionParams


