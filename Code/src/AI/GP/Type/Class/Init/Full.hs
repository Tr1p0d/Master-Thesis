{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Class.Init.Full where

import Control.Lens (makeLenses)
import Data.Int (Int)

data Full = Full
    { _depth :: Int
    , _populationSize :: Int
    }
makeLenses ''Full

