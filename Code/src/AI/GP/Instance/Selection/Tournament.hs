{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Instance.Selection.Tournament where

import AI.GP.Class.SelectionMethod (SelectionMethod, select)
import AI.GP.Type.Class.Selection.Tournament (Tournament)

instance SelectionMethod Tournament where
    select _tournament = let a=a in a
