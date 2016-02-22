{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Class.Init.Full where

import Control.Lens (makeLenses)
import Data.Int (Int)

data Full = Full
    { _depth :: Int
    }
makeLenses ''Full

--fullMethod :: (MonadRandom m) => Int -> m E
--fullMethod 0 = Const <$> getRandom
--fullMethod n = sample [Plus] <*> subExpr <*> subExpr
--  where
--    subExpr = fullMethod $ n - 1
