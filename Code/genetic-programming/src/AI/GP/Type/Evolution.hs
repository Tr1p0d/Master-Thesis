{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP.Type.Evolution where

import Prelude (Double, even, div, (-), (+))
import Debug.Trace (traceM)

import Control.Applicative (Applicative, (<$>))
import Control.Monad (Monad((>>=), return), when)
import Data.Bool (Bool, (||))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((++))
import Data.Ord ((<=))
import Text.Show (Show(show))

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Lazy (StateT, get, gets, modify, execStateT)
import qualified Data.Vector as V
    ( Vector
--    , (!?)
    , (++)
    , drop
    , empty
    , head
    , length
    , mapM
    , replicateM
    , take
    )
import qualified Data.Vector.Generic as IV (unsafeThaw, unsafeFreeze)

import Control.Lens ((^.), (.~), (%~), makeLenses)
import qualified Data.Vector.Algorithms.Merge as Merge (sort)
import System.Random.MWC (Gen)

import AI.GP.Type.Fitnesse
    ( EvaluatedIndividual
    , evalIndividual
    , individualScore
    , _individualScore
    )
import AI.GP.Type.GProgram
    ( Individual
    , IndividualPair
    , getArbitraryPair
    )
import AI.GP.Mutation (stdMute)
import AI.GP.Utils (flatten{-, arbitraryUniformVector, arbitraryVector-})


data EvolutionProgress op t = EvolutionProgress
    { _individual :: EvaluatedIndividual op t
    , _inGeneration :: Int
    }
    deriving (Show)
makeLenses ''EvolutionProgress

data EvolutionState m op t = EvolutionState
    { _generation :: Int
    , _population :: V.Vector (EvaluatedIndividual op t)
    , _selection :: V.Vector (Individual op t)
    , _breed :: V.Vector (Individual op t)
    , _mutated :: V.Vector (Individual op t)
    , _randomToken :: Gen (PrimState m)
    , _bestScore :: Double
    , _evolutionProgress :: [EvolutionProgress op t]
    }
makeLenses ''EvolutionState

instance (Show op, Show t) => Show (EvolutionState m op t) where
    show EvolutionState{..} =
        "DEBUG GP generation : " ++ show _generation ++ "\n" ++
        "Population : " ++ show _population ++ "\n\n"


newtype Evolution op t m a =
    Evolution { runEvolution :: StateT (EvolutionState m op t) m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (Evolution op t) where
    lift = Evolution . lift

debugState :: (Show op, Show t, PrimMonad m)
    => Evolution op t m ()
debugState = Evolution get >>= traceM . show



evalEvolution :: (PrimMonad m)
    => Evolution op t m ()
    -> Gen (PrimState m)
    -> m [EvolutionProgress op t]
evalEvolution e token = do
    _evolutionProgress <$> execStateT (runEvolution e) (emptyState token)

incrementGeneration :: (Monad m) => Evolution op t m ()
incrementGeneration = Evolution $ modify (generation %~ (+1))

emptyState :: (PrimMonad m) => Gen (PrimState m) -> EvolutionState m op t
emptyState token = EvolutionState 0 V.empty V.empty V.empty V.empty token 0.0 []

putProgress
    :: (Monad m)
    => Evolution op t m ()
putProgress = Evolution $ do
    ind <- V.head <$> gets _population
    gen <- gets _generation
    modify (evolutionProgress %~ (EvolutionProgress ind gen:))
    modify (bestScore .~ (ind ^. individualScore))

eventuallyPutProgress
    :: (Monad m)
    => Evolution op t m ()
eventuallyPutProgress = do
    actualBest <- Evolution $ _individualScore . V.head <$> gets _population
    previousBest <- Evolution $ gets _bestScore
    when (actualBest <= previousBest) putProgress

putGeneration
    :: (Monad m)
    => V.Vector (EvaluatedIndividual op t)
    -> Evolution op t m ()
putGeneration gen = Evolution $ modify (population .~ gen)

terminationCondition
    :: (Monad m)
    => (Double -> Bool)
    -> Int
    -> Evolution op t m Bool
terminationCondition suffCond maxG = Evolution $ do
    currentG <- gets _generation
    bestF <- gets _bestScore
    return $ suffCond bestF || currentG == maxG

evalPopulation
    :: (PrimMonad m)
    => (Individual op t -> m Double)
    -> (Double -> Bool)
    -> V.Vector (Individual op t)
    -> Evolution op t m ()
evalPopulation eval tCond pop =
    lift (V.mapM (evalIndividual eval tCond) pop >>= sortPopulation)
    >>= putGeneration

sortPopulation
    :: (PrimMonad m)
    => V.Vector (EvaluatedIndividual op t)
    -> m (V.Vector (EvaluatedIndividual op t))
sortPopulation iv = do
    mv <- IV.unsafeThaw iv
    Merge.sort mv
    IV.unsafeFreeze mv

select
    :: (Monad m)
    => (V.Vector (EvaluatedIndividual op t) -> m (V.Vector (Individual op t)))
    -> Evolution op t m ()
select method = do
    pop <- Evolution $ gets _population
    sel <- lift $ method pop
    Evolution $ modify (selection .~ sel)

cross
    :: (PrimMonad m)
    => Int -- ^ The breed size
    -> (IndividualPair op t -> m (IndividualPair op t))
    -> Evolution op t m ()
cross breedSize crossoverMethod = do
    token <- Evolution $ gets _randomToken
    sel <- Evolution $ gets _selection
    crossed <- lift $ if even breedSize
        then flatten <$> cross' sel (breedSize `div` 2) token
        else V.drop 1 . flatten <$> cross' sel ((breedSize `div` 2) + 1) token
    Evolution $ modify (breed .~ crossed)
  where
    cross' sel size token = V.replicateM
        size
        (getArbitraryPair sel token >>= crossoverMethod)

mute
    :: (PrimMonad m)
    => Double -- ^ Mutation probability
    -> (Individual op t -> m (Individual op t))
    -> Evolution op t m ()
mute probability method = do
    token <- Evolution $ gets _randomToken
    crossed <- Evolution $ gets _breed
    muted <- lift $ V.mapM (stdMute' token) crossed
    Evolution $ modify (mutated .~ muted)
  where
    stdMute' = stdMute probability method

replenish
    :: (PrimMonad m)
    => (V.Vector (EvaluatedIndividual op t) -> m (V.Vector (Individual op t)))
    -> Evolution op t m (V.Vector (Individual op t))
replenish method = do
    mut <- Evolution $ gets _mutated
    pop <- Evolution $ gets _population
    ellitist <- lift $ V.take (V.length pop - V.length mut) <$> method pop
    lift $ return $ mut V.++ ellitist
