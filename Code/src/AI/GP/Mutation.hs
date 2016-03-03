{-# LANGUAGE NoImplicitPrelude #-}

module AI.GP.Mutation where

import Control.Applicative ((<$>))
import Control.Monad (Monad(return))
import Data.Function ((.), const)
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing))

import AI.GP.Type.GProgram (GProgram, withOperation)
import AI.GP.Type.GPZipper
    ( GPZipper
    , fromGPZipper
    , subZippers
    , toGPZipper
    , withFocus
    )
--import Data.Random (MonadRandom, sample)
--import Data.Random.Distribution.Bernoulli (bernoulli)
--import Data.Random.Distribution.Uniform (uniform)

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
