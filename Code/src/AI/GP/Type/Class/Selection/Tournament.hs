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

{- This thing goes later to the instances folder
tournament
    :: (MonadRandom m)
    => Int -- | Tournament rounds
    -> Int -- | Tournament size
    -> V.Vector (Double, e)
    -> m (V.Vector e)
tournament _r s ep = V.replicateM s (randomElem ep)
-}
