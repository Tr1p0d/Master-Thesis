{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.GP where

import AI.GP.Class.Population (Population)

--import Control.Monad.Random (getRandom, getRandomR)
import Control.Monad.Random.Class (MonadRandom)

import Control.Lens ((^.), makeLenses)
--import qualified Data.Vector as V ((!), Vector, length, replicateM)

--randomElem :: MonadRandom m => V.Vector (Double, a) -> m a
--randomElem p = (snd . (p V.!)) <$> getRandomR (0, V.length p)
--
--fullMethod :: (MonadRandom m) => Int -> m E
--fullMethod 0 = Const <$> getRandom
--fullMethod n = sample [Plus] <*> subExpr <*> subExpr
--  where
--    subExpr = fullMethod $ n - 1
--
--data E
--    = Plus E E
--    | Const Double

--tournament
--    :: (MonadRandom m)
--    => Int -- | Tournament rounds
--    -> Int -- | Tournament size
--    -> V.Vector (Double, e)
--    -> m (V.Vector e)
--tournament _r s ep = V.replicateM s (randomElem ep)

--class EvolutionParams param where
--    initialPopulation :: (Initial i) => i -> prms
--    selection :: (Selection s) => s -> prms
--    breed :: (Breed b) => b -> prms

data Evolution i s b mut (p :: * -> *) e = Evolution
    { _initMethod :: i
    , _selectionMethod  :: s
    , _breedMethod :: b
    , _mutationMethod :: mut
    , _terminationCondition :: Double -> Bool
    }
makeLenses ''Evolution

data Selection m p e = Selection
    { _getSelection :: m (p e)
    }
makeLenses ''Selection

data Breed m p e = Breed
    { _getBreed :: m (p e)
    }
makeLenses ''Breed

class SelectionMethod s where
    select
        :: (MonadRandom m, Population p, Program e)
        => s -> Selection m p e

class BreedMethod b where
    cross
        :: (MonadRandom m, Population p, Program e)
        => b -> Selection m p e -> Breed m p e

type RandomNonterminal m e = m (e -> e -> e)
type RandomTerminal m e = m e

class Program e where

    rT :: MonadRandom m => RandomNonterminal m e

    rNT :: MonadRandom m => RandomTerminal m e

    eval :: e -> Double

class InitMethod i where
    init :: (MonadRandom m, Population p, Program e) => i -> m (p e)

class MutationMethod mut where
    mutate
        :: (MonadRandom m, Population p, Program e)
        => mut -> Breed m p e -> m (p e)

data EvolutionState = EvolutionState
    { _getGeneration :: Int
    , _getFittest :: Double
    }
makeLenses ''EvolutionState

evolve
    ::  ( BreedMethod b
        , InitMethod i
        , MutationMethod mut
        , Population p
        , Program e
        , SelectionMethod s
        , MonadRandom m
        )
    => Evolution i s b mut p e -> m EvolutionState
evolve e = AI.GP.init (e ^. initMethod) >>= evolveWithPopulation e

evolveWithPopulation
    :: Evolution i s b mut p e
    -> p e
    -> m EvolutionState
evolveWithPopulation = let a = a in a
