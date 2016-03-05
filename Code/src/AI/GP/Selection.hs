{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Selection where

import AI.GP.Type.GProgram (GProgram)
import AI.GP.Utils (arbitrary)

tournamentGen
    :: (Monad m)
    => Int -- ^ Tournament size
    -> Int -- ^ Tournament rounds
    -> GPPopulation 'Generation (GProgram op t)
    -> m (GPPopulation 'Evaluated (Fitness (GProgram op t)))
tounrnamentGen size rounds population =
