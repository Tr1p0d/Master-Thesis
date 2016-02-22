{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Class.Selection.Tournament where

import Control.Lens (makeLenses)
import Data.Int (Int)

data Tournament = Tournament
    { _rounds :: Int
    , _roundSize :: Int
    }
makeLenses ''Tournament

