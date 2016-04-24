{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Mutation where

import Prelude (Double, error)

import Control.Applicative ((<$>))
import Control.Monad (Monad(return))
import Data.Function ((.), const)
import Data.Int (Int)

import Control.Monad.Primitive (PrimMonad, PrimState)

import Control.Lens ((^.))
import System.Random.MWC (Gen, uniformR)
import System.Random.MWC.Distributions (bernoulli)

import AI.GP.Init (growInit)
import AI.GP.Type.GProgram (GProgram, Individual, height, withOperation)
import AI.GP.Type.GPZipper
    ( GPZipper
    , fromGPZipper
    , subZippers
    , toGPZipper
    , withFocus
    )
import AI.GP.Utils (arbitraryUniform)


stdMute
    :: (PrimMonad m)
    => Double
    -> (GProgram op t -> m (GProgram op t))
    -> Gen (PrimState m)
    -> GProgram op t
    -> m (GProgram op t)
stdMute prob mutM token individual = do
    mute <- bernoulli prob token
    if mute
    then mutM individual
    else return individual

-- {{{ POINT MUTATION ---------------------------------------------------------

pointMutationPreferLeafs
    :: (PrimMonad m)
    => [op]
    -> Double -- ^ Percentil of leaf preference.
    -> Gen (PrimState m)
    -> GProgram op t
    -> m (GProgram op t)
pointMutationPreferLeafs ops prob token prog = do
    leaf <- bernoulli prob token
    if leaf then muteLeaf else muteNode
  where
    muteLeaf = pointMutationLeaf ops token prog
    muteNode = pointMutationUniformNode (prog ^. height) ops token prog

pointMutationUniformNode
    :: (PrimMonad m)
    => Int -- ^ Upper bound
    -> [op] -- ^ Operation set
    -> Gen (PrimState m)
    -> GProgram op t
    -> m (GProgram op t)
pointMutationUniformNode ub ops token program =
    pointMutationGen program (uniformHeight ub token)
        (arbitraryUniform token)
        (arbitraryUniform token ops)

pointMutationLeaf
    :: (PrimMonad m)
    => [op]
    -> Gen (PrimState m)
    -> GProgram op t
    -> m (GProgram op t)
pointMutationLeaf ops token program =
    pointMutationGen program (return 0)
        (arbitraryUniform token)
        (arbitraryUniform token ops)

pointMutationGen
    :: (Monad m)
    => GProgram op t
    -> m Int -- ^ Height of muted node
    -> ([GPZipper op t] -> m (GPZipper op t)) -- ^ Muted node selector
    -> m op -- ^ Substituting operation
    -> m (GProgram op t)
pointMutationGen p heightG nodeS operationG = do
    d <- heightG
    case subZippers d (toGPZipper p) of
        [] -> error "Point Mutation : There are no such subzippers."
        a -> do
            op <- operationG
            fromGPZipper . withFocus (withOperation (const op)) <$> nodeS a

-- }}} POINT MUTATION ---------------------------------------------------------
-- {{{ SUBTREE MUTATION -------------------------------------------------------

subtreeMutationUniform
    :: (PrimMonad m)
    => [op]
    -> [t]
    -> Gen (PrimState m)
    -> Individual op t
    -> m (Individual op t)
subtreeMutationUniform ops terms  token prog = do
    let ub = prog ^. height
    h <- uniformHeight ub token
    st <- growInit ops terms h token
    subtreeMutationGen prog
        h
        (arbitraryUniform token)
        st

subtreeMutationGen
    :: (Monad m)
    => GProgram op t
    -> Int  -- ^ Height of muted node it is without the m since the value
            -- is used somewhere else too.
    -> ([GPZipper op t] -> m (GPZipper op t)) -- ^ Muted node selector
    -> GProgram op t -- ^ Substituing subtree
    -> m (GProgram op t)
subtreeMutationGen prog h nodeS subtree = do
    case subZippers h (toGPZipper prog) of
        [] -> error "SubTree Mutation : There are no such subzippers."
        a -> fromGPZipper . withFocus (const subtree) <$> nodeS a

-- }}} SUBTREE MUTATION -------------------------------------------------------

uniformHeight ub = uniformR (1, ub)
