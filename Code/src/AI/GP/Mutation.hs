{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Mutation where

import Prelude (Float)

import Control.Applicative ((<$>))
import Control.Monad (Monad(return))
import Data.Function (($), (.), const)
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing))

import Control.Lens ((^.))
import Data.Random (MonadRandom, sample)
import Data.Random.Distribution.Bernoulli (bernoulli)
import Data.Random.Distribution.Uniform (uniform)

import AI.GP.Type.GProgram (GProgram, height, withOperation)
import AI.GP.Type.GPZipper
    ( GPZipper
    , fromGPZipper
    , subZippers
    , toGPZipper
    , withFocus
    )
import AI.GP.Utils (arbitraryUniform)

stdMute
    :: (MonadRandom m)
    => Float
    -> (GProgram op t -> m (GProgram op t))
    -> GProgram op t
    -> m (GProgram op t)
stdMute prob mutM individual = do
    mute <- sample $ bernoulli prob
    if mute
    then mutM individual
    else return individual

pointMutationPreferLeafs
    :: (MonadRandom m)
    => GProgram op t
    -> [op]
    -> Float -- ^ Percentil of leaf preference.
    -> m (Maybe (GProgram op t))
pointMutationPreferLeafs prog op prob = do
    leaf <- sample $ bernoulli prob
    if leaf then muteLeaf else muteNode
  where
    muteLeaf = pointMutationLeaf prog op
    muteNode = pointMutationUniformNode prog (prog ^. height) op

pointMutationUniformNode
    :: (MonadRandom m)
    => GProgram op t
    -> Int -- ^ Upper bound
    -> [op] -- ^ Operation set
    -> m (Maybe (GProgram op t))
pointMutationUniformNode program ub =
    pointMutationGen program uniformHeight arbitraryUniform . arbitraryUniform
  where
    uniformHeight = sample $ uniform 1 ub

pointMutationLeaf
    :: (MonadRandom m)
    => GProgram op t
    -> [op]
    -> m (Maybe (GProgram op t))
pointMutationLeaf program =
    pointMutationGen program (return 0) arbitraryUniform . arbitraryUniform

pointMutationGen
    :: (Monad m)
    => GProgram op t
    -> m Int -- ^ Height of muted node
    -> ([GPZipper op t] -> m (GPZipper op t)) -- ^ Muted node selector
    -> m op -- ^ Substituting operation
    -> m (Maybe (GProgram op t))
pointMutationGen p heightG nodeS operationG = do
    d <- heightG
    case subZippers d (toGPZipper p) of
        [] -> return Nothing
        a -> do
            op <- operationG
            reWrap . withFocus (withOperation (const op)) <$> nodeS a
  where
    reWrap = Just . fromGPZipper
